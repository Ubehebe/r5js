goog.provide('r5js.OutputSavingPort');


goog.require('r5js.OutputPort');



/**
 * @param {function(!r5js.runtime.Value):T} writeConverter Function used to
 * convert values passed to (write x).
 * @param {function(!r5js.runtime.Value):T=} opt_displayConverter Function used
 * to convert values passed to (display x). If not given, writeConverter
 * will be used.
 * @implements {r5js.OutputPort}
 * @struct
 * @constructor
 * @template T
 */
r5js.OutputSavingPort = function(writeConverter, opt_displayConverter) {
  /** @const @private */ this.writeConverter_ = writeConverter;
  /** @const @private */ this.displayConverter_ =
      opt_displayConverter || writeConverter;
  /** @private {!Array.<T>} */ this.values_ = [];
};
r5js.OutputPort.addImplementation(r5js.OutputSavingPort);


/** @override */
r5js.OutputSavingPort.prototype.write = function(value) {
  this.values_.push(this.writeConverter_(value));
};


/** @override */
r5js.OutputSavingPort.prototype.display = function(value) {
  this.values_.push(this.displayConverter_(value));
};


/** @override */
r5js.OutputSavingPort.prototype.close = goog.nullFunction;


/** @return {!Array.<T>} */
r5js.OutputSavingPort.prototype.getAndClearOutput = function() {
  var values = this.values_;
  this.values_ = [];
  return values;
};


