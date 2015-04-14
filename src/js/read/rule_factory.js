goog.module('r5js.read.RuleFactory');

const bnf = goog.require('r5js.read.bnf');
const Datum = goog.require('r5js.Datum');
const Grammar = goog.require('r5js.read.Grammar');
const Nonterminal = goog.require('r5js.parse.Nonterminal');

class RuleFactory {
    /** @param {!Grammar} grammar */
    constructor(grammar) {
        this.grammar_ = grammar;
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
     * @param {function(new: Datum, ?)} ctor
     * @return {!Rule}
     */
    onePrimitive(ctor) {
        return bnf.onePrimitive(ctor);
    }

    /**
     * @param {...!Rule} var_args
     * @return {!Rule}
     */
    seq(var_args) {
        return bnf.seq(arguments);
    }

    /**
     * @param {...!Rule} var_args
     * @return {!Rule}
     */
    choice(var_args) {
        return bnf.choice(arguments);
    }
}