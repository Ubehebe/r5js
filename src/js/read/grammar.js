goog.module('r5js.read.Grammar');

const Rule = goog.require('r5js.read.bnf.Rule');

/** @interface */
class Grammar {
    /**
     * @param {string} nonterminal TODO bl make this an actual Nonterminal
     * @return {!Rule}
     */
    ruleFor(nonterminal) {}
}

exports = Grammar;