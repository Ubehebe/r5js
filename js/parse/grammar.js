goog.module('r5js.parse.Grammar');

const Rule = goog.require('r5js.parse.bnf.Rule');
const {Nonterminal} = goog.require('r5js.parse.Nonterminals');

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