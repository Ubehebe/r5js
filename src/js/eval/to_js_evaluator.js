goog.provide('r5js.ToJsEvaluator');



/**
 * @param {!r5js.Evaluator} evaluator
 * @implements {r5js.EvaluateToExternalRepresentation.<?>}
 * @struct
 * @constructor
 */
r5js.ToJsEvaluator = function(evaluator) {
  this.evaluator_ = evaluator;
};


/** @override */
r5js.ToJsEvaluator.prototype.evaluate = function(input) {
  return this.evaluator_.evaluate(input); // TODO bl
};
