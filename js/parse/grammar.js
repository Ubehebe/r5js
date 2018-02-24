goog.module('r5js.parse.Grammar');

const Rule = goog.require('r5js.parse.bnf.Rule');
const {Nonterminal} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');

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