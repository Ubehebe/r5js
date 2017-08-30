goog.module('r5js.async.Evaluator.Impl');

const AsyncEvaluator = goog.require('r5js.async.Evaluator');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const Promise = goog.require('goog.Promise');
const SchemeSources = goog.require('r5js.SchemeSources');
const SyncEvaluator = goog.require('r5js.sync.Evaluator');
const boot = goog.require('r5js.boot');

/**
 * @param {!InputPort=} inputPort
 * @param {!OutputPort=} outputPort
 * @return {!AsyncEvaluator}
 */
function newAsyncEvaluator(inputPort=InputPort.NULL, outputPort=OutputPort.NULL) {
    const sources = new SchemeSources();
    const syncEvaluator = boot(sources.syntax, sources.procedures, inputPort, outputPort);
    return new EvaluatorImpl(syncEvaluator);
}

/**
 * AsyncEvaluator implementation that simply wraps a synchronous evaluator in promises.
 * @implements {AsyncEvaluator}
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

exports = newAsyncEvaluator;
