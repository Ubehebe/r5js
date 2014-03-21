goog.provide('r5js.Token');



/** @interface */
r5js.Token = function() {};


/**
 * @return {!r5js.Datum|!r5js.parse.Terminal} A Datum representing this token,
 * or a Terminal if this token is a terminal (and so has no interesting state).
 * TODO bl: this interface is so small now that it might make sense to eliminate
 * it and have the scanner return datums directly.
 */
r5js.Token.prototype.toDatum = function() {};



