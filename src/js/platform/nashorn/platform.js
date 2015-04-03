goog.provide('r5js.curPlatform');

goog.require('r5js.Platform');
goog.require('r5js.platform.common.newEvaluator');

r5js.platform.Nashorn_ = /** @private @implements {r5js.Platform} */ class {
 /** @override */
 exit() {}

 /** @override */
 newEvaluator(opt_inputPort, opt_outputPort) {
  return r5js.platform.common.newEvaluator(opt_inputPort, opt_outputPort);
 }
};

/** @return {!r5js.Platform} */
r5js.curPlatform = function() {
  return new r5js.platform.Nashorn_();
};
