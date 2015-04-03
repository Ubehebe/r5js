goog.provide('r5js.platform.common.newEvaluator');

goog.require('goog.Promise');
goog.require('r5js.Evaluator');
goog.require('r5js.SchemeSources');
goog.require('r5js.boot');

/**
 * @param {!r5js.InputPort=} opt_inputPort
 * @param {!r5js.OutputPort=} opt_outputPort
 * @return {!goog.Promise<!r5js.Evaluator>}
 */
r5js.platform.common.newEvaluator =
    function(opt_inputPort, opt_outputPort) {
  return r5js.SchemeSources.get().then(function(sources) {
    return r5js.boot(
        sources.syntax,
        sources.procedures,
        opt_inputPort,
        opt_outputPort);
  }).then(function(syncEvaluator) {
    return new r5js.platform.common.Evaluator_(syncEvaluator);
  });
};

/**
 * Evaluator implementation that simply wraps a synchronous evaluator
 * in promises. This is appropriate for most non-web platforms, since these
 * typically can run JavaScript synchronously off the main thread.
 */
r5js.platform.common.Evaluator_ = /** @private @implements {r5js.Evaluator} */ class {
    /** @param {!r5js.sync.Evaluator} evaluator */
    constructor(evaluator) {
        /** @const @private */ this.evaluator_ = evaluator;
    }

    /** @override */
    evaluate(input) {
        try {
            return goog.Promise.resolve(this.evaluator_.evaluate(input));
        } catch (e) {
            return goog.Promise.reject(e);
        }
    }
};
