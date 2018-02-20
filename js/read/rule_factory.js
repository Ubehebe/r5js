goog.module('r5js.read.RuleFactory');

const {Datum, VACUOUS_PROGRAM} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {SimpleDatum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/simple_datum');
const Grammar = goog.require('r5js.read.Grammar');
const Quasiquote = goog.require('r5js.ast.Quasiquote');
const Quote = goog.require('r5js.ast.Quote');
const Rule = goog.require('r5js.read.bnf.Rule');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const TokenStream = goog.require('r5js.TokenStream');
const Unquote = goog.require('r5js.ast.Unquote');
const UnquoteSplicing = goog.require('r5js.ast.UnquoteSplicing');
const {List} = goog.require('r5js.ast.List');
const {Nonterminal} = require('/js/parse/nonterminals_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');
const {Terminals} = require('/js/parse/terminals_collect_es6_sources.es6/node_modules/__main__/js/parse/terminals');

class RuleFactory {
    /** @param {!Grammar} grammar */
    constructor(grammar) {
        this.grammar_ = grammar;
    }

    /**
     * @param {!Terminal|!Nonterminal} terminalOrNonterminal
     * @return {!Rule}
     */
    one(terminalOrNonterminal) {
        return new One(terminalOrNonterminal, this.grammar_);
    }

    /**
     * @param {!Nonterminal} nonterminal
     * @return {!Rule}
     */
    zeroOrMore(nonterminal) {
        return new AtLeast(nonterminal, 0, this.grammar_);
    }

    /**
     * @param {!Nonterminal} nonterminal
     * @return {!Rule}
     */
    oneOrMore(nonterminal) {
        return new AtLeast(nonterminal, 1, this.grammar_);
    }

    /**
     * @param {function(new: Datum, ?)} ctor
     * @return {!Rule}
     */
    onePrimitive(ctor) {
        return new OnePrimitive(ctor);
    }

    /**
     * @param {...!Rule} rules
     * @return {!Seq}
     */
    seq(...rules) {
        return new Seq(rules);
    }

    /**
     * @param {...!Rule} rules
     * @return {!Rule}
     */
    choice(...rules) {
        return new Choice(rules);
    }
}

/** @implements {Rule} */
class One {
    /**
     * @param {!Terminal|!Nonterminal} type
     * @param {!Grammar} grammar
     */
    constructor(type, grammar) {
        /** @const @private */ this.type_ = type.toString();
        /** @const @private */ this.grammar_ = grammar;
    }

    /** @override */
    match(tokenStream) {
        // The rule will be found in the grammar iff it is a nonterminal.
        const rule = this.grammar_.ruleFor(this.type_);
        return rule ? rule.match(tokenStream) : this.matchTerminal_(tokenStream);
    }

    /**
     * @param {!TokenStream} tokenStream
     * @return {?Datum}
     * @private
     */
    matchTerminal_(tokenStream) {
        const token = tokenStream.nextToken();
        return token === this.type_ ? TERMINAL_SENTINEL : null;
    }
}

/**
 * When a terminal is successfully matched out of the token stream,
 * the rule needs to communicate success (= a non-null return value),
 * but the terminal itself is not useful. This sentinel is returned by
 * {@link r5js.read.bnf.One_#matchTerminal_} to avoid instantiating
 * useless datums.
 * TODO bl: this API can be improved.
 * @const {!Datum}
 */
const TERMINAL_SENTINEL = new Datum();

/** @implements {Rule} */
class AtLeast {
    /**
     * @param {!Terminal|!Nonterminal} type
     * @param {number} minRepetitions
     * @param {!Grammar} grammar
     */
    constructor(type, minRepetitions, grammar) {
        /** @const @private */ this.type_ = type.toString();
        /** @const @private */ this.repetition_ = minRepetitions;
        /** @const @private */ this.grammar_ = grammar;
    }

    /** @override */
    match(tokenStream) {
        const siblingBuffer = new SiblingBuffer();
        const rule = this.grammar_.ruleFor(this.type_);
        const checkpoint = tokenStream.checkpoint();
        let num = 0, cur;

        while (cur = rule.match(tokenStream)) {
            siblingBuffer.appendSibling(cur);
            ++num;
        }

        if (num >= this.repetition_) {
            /* In the special case when repetition_ is 0 and 0 datums were
             (successfully) matched, siblingBuffer.toSiblings() will return null.
             However, null is used by this API to communicate failure, so we must
             return a different object. */
            return siblingBuffer.toSiblings() || VACUOUS_PROGRAM;
        } else {
            tokenStream.restore(checkpoint);
            return null;
        }
    }
}

/** @implements {Rule} */
class OnePrimitive {
    /** @param {function(new: Datum, ?)} ctor */
    constructor(ctor) {
        /** @const @private */ this.ctor_ = ctor;
    }

    /** @override */
    match(tokenStream) {
        const token = tokenStream.nextToken();
        return token instanceof this.ctor_ ? token : null;
    }
}

/** @implements {Rule} */
class Seq {
    /** @param {!Array<!Rule>} rules */
    constructor(rules) {
        /** @const @private */ this.rules_ = rules;
        /** @private {function(new: Datum, !Datum)|null} */ this.ctor_ = null;
    }

    /**
     * @param {function(new: Datum, !Datum)} ctor
     * @return {!Seq} This rule, for chaining.
     */
    named(ctor) {
        this.ctor_ = ctor;
        return this;
    }

    /** @override */
    match(tokenStream) {
        const siblingBuffer = new SiblingBuffer();
        const checkpoint = tokenStream.checkpoint();
        for (let i = 0; i < this.rules_.length; ++i) {
            const rule = this.rules_[i];
            const parsed = rule.match(tokenStream);
            if (parsed === TERMINAL_SENTINEL) {
                continue;
            } else if (parsed === VACUOUS_PROGRAM) {
                continue;
            } else if (parsed) {
                siblingBuffer.appendSibling(parsed);
            } else {
                tokenStream.restore(checkpoint);
                return null;
            }
        }

        return Seq.maybeCanonicalize(siblingBuffer.toList(
            /** @type {function(new: Datum, !Datum)} */ (this.ctor_)));
    }

    /**
     * (quote x) -> 'x
     * (quasiquote x) -> `x
     * (unquote x) -> ,x
     * (unquote-splicing x) -> ,@x
     * @param {!Datum} datum
     * @return {!Datum}
     */
    static maybeCanonicalize(datum) {
        if (!(datum instanceof List) || !datum.getFirstChild()) {
            return datum;
        }
        const firstChildToStrip = datum.getFirstChild();
        if (!(firstChildToStrip instanceof SimpleDatum)) {
            return datum;
        }
        const realFirstChild = /** @type {!Datum} */ (firstChildToStrip.getNextSibling());
        switch (firstChildToStrip.getPayload()) {
            case Terminals.QUOTE:
                return new Quote(realFirstChild);
            case Terminals.QUASIQUOTE:
                return new Quasiquote(realFirstChild);
            case Terminals.UNQUOTE:
                return new Unquote(realFirstChild);
            case Terminals.UNQUOTE_SPLICING:
                return new UnquoteSplicing(realFirstChild);
            default:
                return datum;
        }
    }
}

/** @implements {Rule} */
class Choice {
    /** @param {!Array<!Rule>} rules */
    constructor(rules) {
        /** @const @private */ this.rules_ = rules;
    }

    /** @override */
    match(tokenStream) {
        for (let i = 0; i < this.rules_.length; ++i) {
            const checkpoint = tokenStream.checkpoint();
            let newDatum;
            if (newDatum = this.rules_[i].match(tokenStream)) {
                return newDatum;
            } else {
                tokenStream.restore(checkpoint);
            }
        }
        return null;
    }
}

exports = RuleFactory;