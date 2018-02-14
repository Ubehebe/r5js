goog.module('r5js.CallbackBackedPort');

const {OutputPort, addOutputPortImpl} = require('/js/io/output_port_collect_es6_sources.es6/node_modules/__main__/js/io/output_port');

class CallbackBackedPort extends OutputPort {
 /**
  * @param {function(string)} onOutput Callback that will be called
  * whenever output is available.
  */
 constructor(onOutput) {
  super();
  /** @const @private */ this.onOutput_ = onOutput;
 }

 /** @override */
 close() {}

 /**
  * @suppress {reportUnknownTypes}
  * @override
  */
 write(str) {
  this.onOutput_(str);
 }
}

addOutputPortImpl(CallbackBackedPort);
exports = CallbackBackedPort;