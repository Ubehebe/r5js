goog.provide('r5js.OutputSavingPort');


goog.require('r5js.OutputPort');



/**
 * @param {function(!r5js.runtime.Value):T} converter
 * @implements {r5js.OutputPort}
 * @struct
 * @constructor
 * @template T
 */
r5js.OutputSavingPort = function(converter) {
  /** @const @private */ this.converter_ = converter;
  /** @private {!Array.<T>} */ this.values_ = [];
};
r5js.OutputPort.addImplementation(r5js.OutputSavingPort);


/** @override */
r5js.OutputSavingPort.prototype.write = function(value) {
  this.values_.push(this.converter_(value));
};


/** @override */
r5js.OutputSavingPort.prototype.close = goog.nullFunction;


/** @return {!Array.<T>} */
r5js.OutputSavingPort.prototype.getAndClearOutput = function() {
  var values = this.values_;
  this.values_ = [];
  return values;
};


