goog.provide('r5js.Repl');



/** @interface */
r5js.Repl = function() {};


/**
 * @param {string} inputLine
 * @return {string|!r5js.Repl.MoreInputRequired} Output string, or
 * {@link r5js.Repl.MORE_INPUT_REQUIRED} if the input cannot yet be completed.
 */
r5js.Repl.prototype.repl = function(inputLine) {};



/**
 * @struct
 * @constructor
 */
r5js.Repl.MoreInputRequired = function() {};


/** @const */
r5js.Repl.MORE_INPUT_REQUIRED = new r5js.Repl.MoreInputRequired();


