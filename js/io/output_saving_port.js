goog.module('r5js.OutputSavingPort');

const {OutputPort} = goog.require('r5js.OutputPort');

/** @interface @extends {OutputPort} */
class OutputSavingPort {
 /** @return {?string} */
 dequeueOutput() {}
}

exports = OutputSavingPort;


