goog.provide('r5js.curPlatform');


goog.require('r5js.Platform');
goog.require('r5js.platform.common.newEvaluator');



/**
 * @implements {r5js.Platform}
 * @struct
 * @constructor
 * @private
 */
r5js.platform.Android_ = function() {};


/** @override */
r5js.platform.Android_.prototype.exit = function(statusCode) {
  AndroidSchemePlatform.exit(statusCode);
};


/** @override */
r5js.platform.Android_.prototype.newEvaluator =
    r5js.platform.common.newEvaluator;


/** @return {!r5js.Platform} */
r5js.curPlatform = function() {
  return new r5js.platform.Android_();
};
