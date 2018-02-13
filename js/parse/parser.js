goog.module('r5js.Parser');

const Datum = goog.require('r5js.Datum');
const {Nonterminal} = require('/js/parse/nonterminals_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');

/** @interface */
class Parser {
 /**
  * @param {!Nonterminal=} nonterminal Optional nonterminal
  * at the root of the parse tree. If omitted, defaults to
  * {@link Nonterminal.Nonterminals.PROGRAM}.
  * @return {?Datum} The root of the parse tree, or null if parsing
  * was unsuccessful.
  */
 parse(nonterminal) {}
}

exports = Parser;
