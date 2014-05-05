goog.provide('r5js.OutputSavingPort');


goog.require('r5js.OutputPort');
goog.require('r5js.ToJsEvaluator');



/**
 * @implements {r5js.OutputPort}
 * @struct
 * @constructor
 */
r5js.OutputSavingPort = function() {
  /** @private {!Array.<boolean|number|string|!Array|undefined>} */
  this.values_ = [];
};
r5js.OutputPort.addImplementation(r5js.OutputSavingPort);


/** @override */
r5js.OutputSavingPort.prototype.write = function(value) {
  this.values_.push(r5js.ToJsEvaluator.schemeToJsValue(value));
};


/** @override */
r5js.OutputSavingPort.prototype.close = goog.nullFunction;


/** @return {!Array.<boolean|number|string|!Array|undefined>} */
r5js.OutputSavingPort.prototype.getAndClearOutput = function() {
  var values = this.values_;
  this.values_ = [];
  return values;
};


