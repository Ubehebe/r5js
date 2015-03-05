goog.provide('r5js.curPlatform');


goog.require('goog.Promise');
goog.require('r5js.Platform');
goog.require('r5js.SchemeSources');
goog.require('r5js.boot');
goog.require('r5js.platform.android.Evaluator');
goog.require('r5js.test.SchemeSources');



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
    function(opt_inputPort, opt_outputPort) {
  return r5js.SchemeSources.get().then(function(sources) {
    return r5js.boot(
        sources.syntax,
        sources.procedures,
        this,
        opt_inputPort,
        opt_outputPort);
  }, undefined /* opt_onRejected */, this).then(function(syncEvaluator) {
    return new r5js.platform.android.Evaluator(syncEvaluator);
  });
};


/** @return {!r5js.Platform} */
r5js.curPlatform = function() {
  return new r5js.platform.Android_();
};
