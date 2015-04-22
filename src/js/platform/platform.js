goog.module('r5js.Platform');

const Evaluator = goog.require('r5js.Evaluator');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');

/**
 * Abstraction of the (JavaScript) platform that the Scheme implementation is running in.
 * @interface
 */
class Platform {
    /** @param {number} statusCode */
    exit(statusCode) {}

    /**
     * @param {!InputPort=} opt_inputPort
     * @param {!OutputPort=} opt_outputPort
     * @return {!Evaluator}
     */
    newEvaluator(opt_inputPort, opt_outputPort) {}
}

exports = Platform;
