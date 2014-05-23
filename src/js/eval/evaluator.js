goog.provide('r5js.Evaluator');



/** @interface */
r5js.Evaluator = function() {};


/**
 * @param {string} input
 * @return {!r5js.runtime.Value}
 */
r5js.Evaluator.prototype.evaluate = function(input) {};


/**
 * The main difference between an evaluator and a read-eval-print loop
 * (REPL) is that a REPL has to deal with user input that is possibly
 * incomplete, for example when a user types half of an expression on one line,
 * presses enter, and completes the expression on the next line.
 * This method arguably belongs in {@link r5js.Repl}, but keeping it here
 * exposes less of the evaluator's guts.
 * @param {string} input
 * @return {boolean} Whether input parses successfully.
 */
r5js.Evaluator.prototype.willParse = function(input) {};


/**
 * @param {!r5js.InputPort} inputPort
 * @param {!r5js.OutputPort} outputPort
 * @return {!r5js.Evaluator} A new evaluator connected to the given ports.
 */
r5js.Evaluator.prototype.withPorts = function(inputPort, outputPort) {};
