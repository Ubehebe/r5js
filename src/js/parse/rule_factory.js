goog.module('r5js.parse.RuleFactory');

const bnf = goog.require('r5js.parse.bnf');
const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const DesugarableRule = goog.require('r5js.parse.bnf.DesugarableRule');
const Grammar = goog.require('r5js.parse.Grammar');
const Rule = goog.require('r5js.parse.bnf.Rule');
const Nonterminal = goog.require('r5js.parse.Nonterminal');

class RuleFactory {

    /** @param {!Grammar} grammar */
    constructor(grammar) {
        /** @const @private */ this.grammar_ = grammar;
    }

    /**
     * @param {!r5js.parse.Terminal|!Nonterminal} terminalOrNonterminal
     * @return {!Rule}
     */
    one(terminalOrNonterminal) {
        return bnf.one(terminalOrNonterminal);
    }

    /**
     * @param {!Nonterminal} nonterminal
     * @return {!Rule}
     */
    zeroOrMore(nonterminal) {
        return bnf.zeroOrMore(nonterminal);
    }

    /**
     * @param {!Nonterminal} nonterminal
     * @return {!Rule}
     */
    oneOrMore(nonterminal) {
        return bnf.oneOrMore(nonterminal);
    }

    /**
     * @param {...!Rule} var_args
     * @return {!Rule}
     */
    choice(var_args) {
        return bnf.choice.apply(null, arguments);
    }

    /**
     * @param {...!Rule} var_args
     * @return {!DesugarableRule}
     */
    seq(var_args) {
        return bnf.seq.apply(null, arguments);
    }

    /**
     * @param {...!Rule} var_args
     * @return {!DesugarableRule<!CompoundDatum>}
     */
    list(var_args) {
        return bnf.list.apply(null, arguments);
    }

    /**
     * @param {!Rule} beforeDot
     * @param {!Rule} afterDot
     * @return {!DesugarableRule<!CompoundDatum>}
     */
    dottedList(beforeDot, afterDot) {
        return bnf.dottedList(beforeDot, afterDot);
    }

    /**
     * @param {...!Rule} var_args
     * @return {!DesugarableRule<!CompoundDatum>}
     */
    vector(var_args) {
        return bnf.vector.apply(null, arguments);
    }

    /**
     * @param {function(!Datum):boolean} predicate
     * @return {!Rule}
     */
    matchDatum(predicate) {
        return bnf.matchDatum(predicate);
    }
}

exports = RuleFactory;