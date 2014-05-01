goog.provide('r5js.ToStringEvaluator');


goog.require('r5js.Datum');
goog.require('r5js.OutputMode');



/**
 * @param {!r5js.Evaluator} evaluator
 * @implements {r5js.EvaluateToExternalRepresentation.<string>}
 * @struct
 * @constructor
 */
r5js.ToStringEvaluator = function(evaluator) {
  /** @const @private */ this.evaluator_ = evaluator;
};


/** @override */
r5js.ToStringEvaluator.prototype.evaluate = function(input) {
  var value = this.evaluator_.evaluate(input);
  return value instanceof r5js.Datum ?
      (/** @type {!r5js.Datum} */ (value)).stringForOutputMode(
      r5js.OutputMode.DISPLAY) :
      (value ? value.toString() : '');
};
