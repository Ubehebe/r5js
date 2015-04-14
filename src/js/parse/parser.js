goog.module('r5js.Parser');

const Datum = goog.require('r5js.Datum');
const Nonterminal = goog.require('r5js.parse.Nonterminal');
const Rule = goog.require('r5js.parse.bnf.Rule');

/** @interface */
class Parser {
 /**
  * @param {!Nonterminal=} opt_nonterminal Optional nonterminal
  * at the root of the parse tree. If omitted, defaults to
  * {@link r5js.parse.Nonterminals.PROGRAM}.
  * @return {Datum} The root of the parse tree, or null if parsing
  * was unsuccessful.
  */
 parse(opt_nonterminal) {}
}

exports = Parser;
