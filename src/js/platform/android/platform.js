goog.module('r5js.curPlatform');

const Platform = goog.require('r5js.Platform');
const newEvaluator = goog.require('r5js.platform.common.newEvaluator');

/** @implements {Platform} */
class Android {
    /** @override */
    exit(statusCode) {
        AndroidSchemePlatform.exit(statusCode);
    }

    /** @override */
    newEvaluator(opt_inputPort, opt_outputPort) {
        return newEvaluator(opt_inputPort, opt_outputPort);
    }
}

/** @return {!Platform} */
function curPlatform() {
  return new Android();
}

exports = curPlatform;
