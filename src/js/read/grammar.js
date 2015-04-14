goog.module('r5js.read.Grammar');

const Nonterminal = goog.require('r5js.parse.Nonterminal');
const Rule = goog.require('r5js.read.bnf.Rule');

/** @interface */
class Grammar {
    /**
     * @param {!Nonterminal} nonterminal
     * @return {!Rule}
     */
    ruleFor(nonterminal) {}
}

exports = Grammar;