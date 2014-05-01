goog.provide('r5js.EvaluateToExternalRepresentation');



/**
 * An evaluator is a function from input strings to Scheme values.
 * (See {@link r5js.Evaluator}.) But to interact with the outside world,
 * Scheme values need to be converted to a suitable external representation.
 * This is a convenience interface representing both the underlying evaluation
 * and the subsequent conversion, saving callers from having to specify a
 * conversion function for each evaluation.
 * @interface
 * @template T
 */
r5js.EvaluateToExternalRepresentation = function() {};


/**
 * @param {string} input
 * @return {T}
 */
r5js.EvaluateToExternalRepresentation.prototype.evaluate = function(input) {};
