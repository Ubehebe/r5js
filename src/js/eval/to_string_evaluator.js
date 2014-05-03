goog.provide('r5js.ToStringEvaluator');


goog.require('r5js.OutputMode');
goog.require('r5js.datumutil');



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
  var wrapped = r5js.datumutil.wrapValue(this.evaluator_.evaluate(input));
  return wrapped.stringForOutputMode(r5js.OutputMode.WRITE);
};
