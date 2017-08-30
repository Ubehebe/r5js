goog.module('r5js.platform.common.newEvaluator');

const Evaluator = goog.require('r5js.Evaluator');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const Promise = goog.require('goog.Promise');
const SchemeSources = goog.require('r5js.SchemeSources');
const SyncEvaluator = goog.require('r5js.sync.Evaluator');
const boot = goog.require('r5js.boot');

/**
 * @param {!InputPort=} opt_inputPort
 * @param {!OutputPort=} opt_outputPort
 * @return {!Evaluator}
 */
function newEvaluator(opt_inputPort, opt_outputPort) {
    const sources = SchemeSources.get();
    const syncEvaluator = boot(sources.syntax, sources.procedures, opt_inputPort, opt_outputPort);
    return new EvaluatorImpl(syncEvaluator);
}

/**
 * Evaluator implementation that simply wraps a synchronous evaluator
 * in promises. This is appropriate for most non-web platforms, since these
 * typically can run JavaScript synchronously off the main thread.
 * @implements {Evaluator}
 */
class EvaluatorImpl {
    /** @param {!SyncEvaluator} evaluator */
    constructor(evaluator) {
        /** @const @private */ this.evaluator_ = evaluator;
    }

    /** @override */
    evaluate(input) {
        try {
            return Promise.resolve(this.evaluator_.evaluate(input));
        } catch (e) {
            return Promise.reject(e);
        }
    }
}

exports = newEvaluator;
