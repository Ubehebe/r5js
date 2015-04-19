goog.module('r5js.platform.Nashorn');

const Platform = goog.require('r5js.Platform');
const newCommonEvaluator = goog.require('r5js.platform.common.newEvaluator');

/** @implements {Platform} */
class Nashorn {
    /** @override */
    exit() {}

    /** @override */
    newEvaluator(opt_inputPort, opt_outputPort) {
        return newCommonEvaluator(opt_inputPort, opt_outputPort);
    }
}

exports = Nashorn;