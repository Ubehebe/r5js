goog.provide('r5js.Evaluator');



/** @interface */
r5js.Evaluator = function() {};


/**
 * @param {string} input
 * @return {!r5js.runtime.Value}
 */
r5js.Evaluator.prototype.evaluate = function(input) {};
