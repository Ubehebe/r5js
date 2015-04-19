goog.module('r5js.curPlatform');

const Nashorn = goog.require('r5js.platform.Nashorn');

/** @return {!Platform} */
function curPlatform() {
    return new Nashorn();
}

exports = curPlatform;
