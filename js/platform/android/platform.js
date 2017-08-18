goog.module('r5js.curPlatform');

const Evaluator = goog.require('r5js.Evaluator');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const Platform = goog.require('r5js.Platform');
const newCommonEvaluator = goog.require('r5js.platform.common.newEvaluator');

/** @implements {Platform} */
class Android {
    /** @override */
    exit(statusCode) {
        AndroidSchemePlatform.exit(statusCode);
    }

    /**
     * @param {!InputPort=} opt_inputPort
     * @param {!OutputPort=} opt_outputPort
     * @return {!Evaluator}
     * @override TODO bl compiler bug?
     */
    newEvaluator(opt_inputPort, opt_outputPort) {
        return newCommonEvaluator(opt_inputPort, opt_outputPort);
    }
}

/** @return {!Platform} */
function curPlatform() {
  return new Android();
}

exports = curPlatform;
