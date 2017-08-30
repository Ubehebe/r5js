goog.module('r5js.Platform');

const Evaluator = goog.require('r5js.Evaluator');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');

/**
 * The JavaScript platform that the Scheme implementation is running in.
 * @interface
 */
class Platform {
    /** @param {number} statusCode */
    exit(statusCode) {}

    /**
     * @param {!InputPort=} inputPort
     * @param {!OutputPort=} outputPort
     * @return {!Evaluator}
     */
    newEvaluator(inputPort, outputPort) {}
}

exports = Platform;
