goog.module('r5js.parse.Grammar');

const {Rule} = require('/js/parse/shim_collect_es6_sources.es6/node_modules/__main__/js/parse/rule');
const {Nonterminal} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');

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