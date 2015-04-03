goog.provide('r5js.platform.html5.OutputPort');

goog.require('r5js.OutputPort');

r5js.platform.html5.OutputPort = /** @implements {r5js.OutputPort} */ class {
 /** @param {function(string)} postMessage */
 constructor(postMessage) {
  /** @const @private */ this.postMessage_ = postMessage;
 }

 /** @override */
 write(value) {
  this.postMessage_(value);
 }

 /** @override */
 close() {}
};

r5js.OutputPort.addImplementation(r5js.platform.html5.OutputPort);
