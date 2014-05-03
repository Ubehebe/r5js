goog.provide('r5js.ToJsEvaluator');


goog.require('r5js.Datum');



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
  var value = this.evaluator_.evaluate(input);
  if (value instanceof r5js.Datum) {
    value = value.unwrap();
  }
  if (value instanceof r5js.ast.String ||
      value instanceof r5js.ast.Character) {
    value = value.getPayload();
  }
  return value;
};
