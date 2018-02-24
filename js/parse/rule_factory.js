goog.module('r5js.parse.RuleFactory');

const {CompoundDatum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/compound_datum');
const {Datum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {SimpleDatum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/simple_datum');
const DesugarableRule = goog.require('r5js.parse.bnf.DesugarableRule');
const Grammar = goog.require('r5js.parse.Grammar');
const Quasiquote = goog.require('r5js.ast.Quasiquote');
const {Quote} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/quote');
const Rule = goog.require('r5js.parse.bnf.Rule');
const {Unquote} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/unquote');
const {UnquoteSplicing} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/unquote_splicing');
const Vector = goog.require('r5js.ast.Vector');
const {DottedList, List} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/list');
const {Nonterminal} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');
const {
    BACKTICK,
    BEGIN,
    COMMA,
    COMMA_AT,
    DEFINE,
    DEFINE_SYNTAX,
    DOT,
    ELLIPSIS,
    IF,
    LAMBDA,
    LET_SYNTAX,
    LETREC_SYNTAX,
    LPAREN,
    LPAREN_DOT,
    LPAREN_VECTOR,
    QUASIQUOTE,
    QUOTE,
    RPAREN,
    SET,
    SYNTAX_RULES,
    TICK,
    UNQUOTE,
    UNQUOTE_SPLICING,
} = require('/js/parse/terminals_collect_es6_sources.es6/node_modules/__main__/js/parse/terminals');

class RuleFactory {

    /** @param {!Grammar} grammar */
    constructor(grammar) {
        /** @const @private */ this.grammar_ = grammar;
    }

    /**
     * @param {!Terminal|!Nonterminal} terminalOrNonterminal
     * @return {!DesugarableRule<?>}
     */
    one(terminalOrNonterminal) {
        return goog.isString(terminalOrNonterminal)
            ? new OneTerminal(terminalOrNonterminal)
            : new OneNonterminal(terminalOrNonterminal, this.grammar_);
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
     * @param {...!Rule} rules
     * @return {!Rule}
     */
    choice(...rules) {
        return new Choice(rules);
    }

    /**
     * @param {...!Rule} rules
     * @return {!DesugarableRule}
     */
    seq(...rules) {
        return new Seq(rules);
    }

    /**
     * @param {...!Rule} var_args
     * @return {!DesugarableRule<!CompoundDatum>}
     */
    list(var_args) {
        const rules = [];
        rules.push(new OneTerminal(LPAREN));
        for (let i = 0; i < arguments.length; ++i) {
            rules.push(arguments[i]);
        }
        rules.push(new OneTerminal(RPAREN));
        return new Seq(rules);
    }

    /**
     * @param {!Rule} beforeDot
     * @param {!Rule} afterDot
     * @return {!DesugarableRule<!CompoundDatum>}
     */
    dottedList(beforeDot, afterDot) {
        const rules = [
            new OneTerminal(LPAREN),
            beforeDot,
            new OneTerminal(DOT),
            afterDot];
        return new Seq(rules);
    }

    /**
     * @param {...!Rule} var_args
     * @return {!DesugarableRule<!CompoundDatum>}
     */
    vector(var_args) {
        const rules = [];
        rules.push(new OneTerminal(LPAREN_VECTOR));
        for (let i = 0; i < arguments.length; ++i) {
            rules.push(arguments[i]);
        }
        rules.push(new OneTerminal(RPAREN));
        return new Seq(rules);
    }

    /**
     * @param {function(!Datum):boolean} predicate
     * @return {!Rule}
     */
    matchDatum(predicate) {
        return new MatchDatum(predicate);
    }
}

/** @implements {DesugarableRule<null>} */
class OneTerminal {
    /** @param {!Terminal} terminal */
    constructor(terminal) {
        /** @const @private */ this.terminal_ = terminal;
    }

    /**
     * TODO: this class isn't really desugarable. It implements
     * DesugarableRule so that one() can return a DesugurableRule consistently.
     * Break up one() into oneTerminal(), returning a Rule, and oneNonterminal(), returning a
     * DesugarableRule.
     * @override
     */
    desugar(desugarFunc) {
    }

    /**
     * @override
     * TODO bl put the instanceof checks into the Datum subclasses
     */
    match(datumStream) {
        if (this.terminal_ === RPAREN) {
            return datumStream.maybeAdvanceToNextSiblingOfParent();
        }

        const next = datumStream.getNextDatum();
        let match = false;
        switch (this.terminal_) {
            case LPAREN:
                match = next instanceof List;
                break;
            case LPAREN_DOT:
                match = next instanceof DottedList;
                break;
            case LPAREN_VECTOR:
                match = next instanceof Vector;
                break;
            case TICK:
                match = next instanceof Quote;
                break;
            case BACKTICK:
                match = next instanceof Quasiquote;
                break;
            case COMMA:
                match = next instanceof Unquote;
                break;
            case COMMA_AT:
                match = next instanceof UnquoteSplicing;
                break;
            default: // TODO bl where is this from?
                if (next instanceof SimpleDatum && next.getPayload() === this.terminal_) {
                    datumStream.advanceToNextSibling();
                    return true;
                } else {
                    return false;
                }
        }

        if (match) {
            datumStream.advanceToChild();
        }
        return match;
    }
}

/** @implements {DesugarableRule<!Datum>} */
class OneNonterminal {
    /**
     * @param {!Nonterminal} nonterminal
     * @param {!Grammar} grammar
     */
    constructor(nonterminal, grammar) {
        /** @const @private */ this.nonterminal_ = nonterminal;
        /** @const @private */ this.grammar_ = grammar;
        /** @private {function(!Datum, !IEnvironment)|null} */ this.desugarFunc_ = null;
    }

    /** @override */
    desugar(desugarFunc) {
        this.desugarFunc_ = desugarFunc;
        return this;
    }

    /** @override */
    match(datumStream) {
        const parsed = this.grammar_.ruleFor(this.nonterminal_).match(datumStream);
        if (parsed instanceof Datum) {
            parsed.setParse(this.nonterminal_);
            if (this.desugarFunc_) {
                parsed.setDesugar(this.desugarFunc_);
            }
            datumStream.advanceTo(/** @type {!Datum} */ (parsed.getNextSibling()));
        }
        return parsed;
    }
}

/** @implements {Rule} */
class AtLeast {
    /**
     * @param {!Nonterminal} nonterminal
     * @param {number} minRepetitions
     * @param {!Grammar} grammar
     */
    constructor(nonterminal, minRepetitions, grammar) {
        /** @const @private */ this.nonterminal_ = nonterminal;
        /** @const @private */ this.minRepetitions_ = minRepetitions;
        /** @const @private */ this.grammar_ = grammar;
    }

    /** @override */
    match(datumStream) {
        let numParsed = 0;
        let parsed;
        while (parsed = this.grammar_.ruleFor(this.nonterminal_).match(datumStream)) {
            parsed.setParse(this.nonterminal_);
            ++numParsed;
        }
        return numParsed >= this.minRepetitions_;
    }
}

/** @implements {Rule} */
class MatchDatum {
    /** @param {function(!Datum):boolean} predicate */
    constructor(predicate) {
        /** @const @private */ this.predicate_ = predicate;
    }

    /** @override */
    match(datumStream) {
        const next = datumStream.getNextDatum();
        if (next && this.predicate_(next)) {
            datumStream.advanceToNextSibling();
            return true;
        } else {
            return false;
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
    match(datumStream) {
        let parsed;
        for (let i = 0; i < this.rules_.length; ++i) {
            const rule = this.rules_[i];
            if (parsed = rule.match(datumStream)) {
                return parsed;
            }
        }
        return false;
    }
}

/** @implements {DesugarableRule<!CompoundDatum>} */
class Seq {
    /** @param {!Array<!Rule>} rules */
    constructor(rules) {
        /** @const @private */ this.rules_ = rewriteImproperList(rules);
        /** @private {function(!CompoundDatum, !IEnvironment)|null} */
        this.desugarFunc_ = null;
    }

    /**
     * @override
     * @suppress {checkTypes} for the type mismatch between this.desugarFunc_
     * and {@link r5js.Datum#setDesugar}. TODO bl.
     */
    match(datumStream) {
        const root = datumStream.getNextDatum();

        for (let i = 0; i < this.rules_.length; ++i) {
            if (!this.rules_[i].match(datumStream)) {
                datumStream.advanceTo(/** @type {!Datum} */ (root));
                return false;
            }
        }

        const nextSibling = /** just in case of an empty program */ root &&
            root.getNextSibling();
        datumStream.advanceTo(/** @type {!Datum} */ (nextSibling));

        if (root instanceof Datum && this.desugarFunc_) {
            root.setDesugar(this.desugarFunc_);
        }

        return root || false;
    }

    /** @override */
    desugar(desugarFunc) {
        this.desugarFunc_ = desugarFunc;
        return this;
    }
}

/**
 * This is a convenience function: we want to specify parse rules like
 * (<variable>+ . <variable>) as if we don't know ahead of time whether
 * the list is going to be dotted or not, but the reader already knows.
 * Proper and improper lists are both represented as first-child-next-sibling
 * linked lists; the only difference is the type ('(' vs. '.('). So we rewrite
 * the parse rules to conform to the reader's knowledge.
 * @param {!Array<!Rule>} rules
 * @return {!Array<!Rule>} The modified rules array.
 * @private
 */
function rewriteImproperList(rules) {
    // example: (define (x . y) 1) => (define .( x . ) 1)
    /* No RHS in the grammar has more than one dot.
     This will break if such a rule is added. */
    const indexOfDot =
        rules.findIndex(rule => rule instanceof OneTerminal && rule.terminal_ === DOT);

    if (indexOfDot === -1) {
        return rules;
    }

    // Find the closest opening paren to the left of the dot and rewrite it as .(
    for (let i = indexOfDot - 1; i >= 0; --i) {
        const rule = rules[i];
        if (rule instanceof OneTerminal && rule.terminal_ === LPAREN) {
            rules[i] = new OneTerminal(LPAREN_DOT);
            break;
        }
    }
    /* Splice out the dot and the datum following the dot -- it has already
     been read as part of the list preceding the dot.
     todo bl: this will cause problems with exactly one part of the grammar:
     <template> -> (<template element>+ . <template>)
     I think it's easier to check for this in the evaluator. */
    rules.splice(indexOfDot, 2);
    return rules;
}

exports = RuleFactory;