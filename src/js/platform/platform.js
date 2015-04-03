goog.provide('r5js.Platform');

/**
 * Abstraction of the (JavaScript) platform that the Scheme implementation
 * is running in.
 */
r5js.Platform = /** @interface */ class {
 /** @param {number} statusCode */
 exit(statusCode) {}

 /**
  * @param {!r5js.InputPort=} opt_inputPort
  * @param {!r5js.OutputPort=} opt_outputPort
  * @return {!goog.Promise<!r5js.Evaluator>}
  */
 newEvaluator(opt_inputPort, opt_outputPort) {}
};
