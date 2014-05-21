goog.provide('r5js.R5RSCompliantOutputPort');

goog.require('r5js.OutputPort');



/**
 * {@link r5js.OutputPort} implementation that uses the official R5RS
 * serializations for writing and displaying Scheme values.
 * @param {function(string)} onOutput Function that will be called whenever
 * output is available.
 * @implements {r5js.OutputPort}
 * @struct
 * @constructor
 */
r5js.R5RSCompliantOutputPort = function(onOutput) {
  /** @const @private */ this.onOutput_ = onOutput;
};
r5js.OutputPort.addImplementation(r5js.R5RSCompliantOutputPort);


/** @override */
r5js.R5RSCompliantOutputPort.prototype.close = goog.nullFunction;


/** @override */
r5js.R5RSCompliantOutputPort.prototype.display = function(value) {
  this.onOutput_(r5js.EvalAdapter.toDisplayString(value));
};


/** @override */
r5js.R5RSCompliantOutputPort.prototype.writeChar = function(c) {
  this.onOutput_(c);
};


/** @override */
r5js.R5RSCompliantOutputPort.prototype.writeValue = function(value) {
  this.onOutput_(r5js.EvalAdapter.toWriteString(value));
};
