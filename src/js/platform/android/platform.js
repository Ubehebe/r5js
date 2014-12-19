goog.provide('r5js.platform.Android');


goog.require('goog.Promise');
goog.require('r5js.InMemoryInputPort');
goog.require('r5js.InMemoryOutputPort');
goog.require('r5js.InMemoryPortBuffer');
goog.require('r5js.SchemeSources');
goog.require('r5js.boot');
goog.require('r5js.platform.android.Terminal');
goog.require('r5js.platform.node.Evaluator');
goog.require('r5js.test.SchemeSources');



/**
 * @implements {r5js.Platform}
 * @struct
 * @constructor
 */
r5js.platform.Android = function() {
  /** @const @private {!Object.<string, !r5js.InMemoryPortBuffer>} */
  this.buffers_ = {};
};


/** @override */
r5js.platform.Android.prototype.getSources = function() {
  return goog.Promise.resolve(new r5js.SchemeSources(
      AndroidSchemePlatform.getR5RSSyntax(),
      AndroidSchemePlatform.getR5RSProcedures()));
};


/** @override */
r5js.platform.Android.prototype.getTestSources = function() {
  return goog.Promise.resolve(new r5js.test.SchemeSources(
      AndroidSchemePlatform.getTestFramework(),
      AndroidSchemePlatform.getTestFrameworkTests(),
      AndroidSchemePlatform.getR5RSTests(),
      AndroidSchemePlatform.getNegativeTests(),
      AndroidSchemePlatform.getOtherTests()));
};


/** @override */
r5js.platform.Android.prototype.exit = function(statusCode) {
  AndroidSchemePlatform.exit(statusCode);
};


/**
 * @param {!r5js.InputPort=} opt_inputPort
 * @param {!r5js.OutputPort=} opt_outputPort
 * @return {!goog.Promise.<!r5js.Evaluator>}
 * @override TODO bl why is it necessary to repeat the doc?
 */
r5js.platform.Android.prototype.newEvaluator =
    function(opt_inputPort, opt_outputPort) {
  return this.getSources().then(function(sources) {
    return r5js.boot(
        sources.syntax,
        sources.procedures,
        this,
        opt_inputPort,
        opt_outputPort);
  }, undefined /* opt_onRejected */, this).then(function(syncEvaluator) {
    return new r5js.platform.node.Evaluator(syncEvaluator);
  });
};


/** @override */
r5js.platform.Android.prototype.getTerminal = function() {
  return new r5js.platform.android.Terminal();
};


/** @override */
r5js.platform.Android.prototype.newInputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = new r5js.InMemoryPortBuffer();
  }
  return new r5js.InMemoryInputPort(this.buffers_[name]);
};


/** @override */
r5js.platform.Android.prototype.newOutputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = new r5js.InMemoryPortBuffer();
  }
  return new r5js.InMemoryOutputPort(this.buffers_[name]);
};