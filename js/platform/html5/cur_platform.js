goog.module('r5js.curPlatform');

const Platform = goog.require('r5js.platform.html5.Platform');

/** @return {!Platform} */
function curPlatform() {
    return new Platform();
}

exports = curPlatform;