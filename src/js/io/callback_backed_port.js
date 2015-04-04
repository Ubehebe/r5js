goog.module('r5js.CallbackBackedPort');

const OutputPort = goog.require('r5js.OutputPort');

/** @implements {OutputPort} */
class CallbackBackedPort {
 /**
  * @param {function(string)} onOutput Callback that will be called
  * whenever output is available.
  */
 constructor(onOutput) {
  /** @const @private */ this.onOutput_ = onOutput;
 }

 /** @override */
 close() {}

 /** @override */
 write(str) {
  this.onOutput_(str);
 }
}

OutputPort.addImplementation(CallbackBackedPort);
exports = CallbackBackedPort;