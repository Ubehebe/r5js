goog.module('r5js.read.Grammar');

const {Rule} = require('/js/read/rule_collect_es6_sources.es6/node_modules/__main__/js/read/rule');

/** @interface */
class Grammar {
    /**
     * @param {string} nonterminal TODO bl make this an actual Nonterminal
     * @return {!Rule}
     */
    ruleFor(nonterminal) {}
}

exports = Grammar;