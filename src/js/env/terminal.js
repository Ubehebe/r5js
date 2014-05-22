goog.provide('r5js.Terminal');



/**
 * Abstraction for a terminal that the Scheme REPL can read to and write from.
 * @interface
 */
r5js.Terminal = function() {};


/** @return {!goog.Promise.<string>} The next line of input. */
r5js.Terminal.prototype.getNextLineOfInput = function() {};


/** @param {string} str String to print. */
r5js.Terminal.prototype.print = function(str) {};


/** @param {string} str Error message to print. */
r5js.Terminal.prototype.error = function(str) {};
