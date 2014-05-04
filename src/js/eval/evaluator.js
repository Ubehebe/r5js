goog.provide('r5js.Evaluator');



/** @interface */
r5js.Evaluator = function() {};


/**
 * @param {string} input
 * @return {!r5js.runtime.Value}
 */
r5js.Evaluator.prototype.evaluate = function(input) {};


/**
 * @param {!r5js.InputPort} inputPort
 * @param {!r5js.OutputPort} outputPort
 * @return {!r5js.Evaluator} A new evaluator connected to the given ports.
 */
r5js.Evaluator.prototype.withPorts = function(inputPort, outputPort) {};
