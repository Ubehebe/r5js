goog.provide('r5js.Terminal');



/**
 * "Dumb" terminal that {@link r5js.Repl} can read to and write from.
 * In contrast to a Repl, a Terminal knows nothing about Scheme.
 * @interface
 */
r5js.Terminal = function() {};


/** @return {!goog.Promise.<string>} The next line of input. */
r5js.Terminal.prototype.getNextLineOfInput = function() {};


/** @param {string} str String to print. */
r5js.Terminal.prototype.print = function(str) {};


/** @param {string} str Error message to print. */
r5js.Terminal.prototype.error = function(str) {};
