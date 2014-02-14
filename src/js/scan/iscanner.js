goog.provide('r5js.IScanner');



/** @interface */
r5js.IScanner = function() {};


/** @return {r5js.Token} The next token, or null if there are no more. */
r5js.IScanner.prototype.nextToken = function() {};
