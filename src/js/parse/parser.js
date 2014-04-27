goog.provide('r5js.Parser');



/** @interface */
r5js.Parser = function() {};


/**
 * @param {!r5js.parse.Nonterminal=} opt_nonterminal Optional nonterminal
 * at the root of the parse tree. If omitted, defaults to
 * {@link r5js.parse.Nonterminals.PROGRAM}.
 * @return {r5js.Datum} The root of the parse tree, or null if parsing
 * was unsuccessful.
 */
r5js.Parser.prototype.parse = function(opt_nonterminal) {};