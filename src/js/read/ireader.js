goog.provide('r5js.IReader');



/** @interface */
r5js.IReader = function() {};


/**
 * @return {r5js.Datum} The root of the datum tree,
 * or null if reading the tokens into datums was unsuccessful.
 */
r5js.IReader.prototype.read = function() {};
