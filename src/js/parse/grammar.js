goog.module('r5js.parse.Grammar');

const Nonterminal = goog.require('r5js.parse.Nonterminal');
const Rule = goog.require('r5js.parse.bnf.Rule');

/**
 * @interface
 * @package
 */
class Grammar {
    /**
     * @param {!Nonterminal} nonterminal
     * @return {!Rule}
     */
    ruleFor(nonterminal) {}
}

exports = Grammar;