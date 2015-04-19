goog.module('r5js.curPlatform');

const Platform = goog.require('r5js.platform.Node');

/** @return {!Platform} */
function curPlatform() {
    return new Platform();
}

exports = curPlatform;