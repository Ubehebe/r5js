goog.module('r5js.async.Evaluator.Impl');

const Evaluator = goog.require('r5js.Evaluator');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const Promise = goog.require('goog.Promise');
const SchemeSources = goog.require('r5js.SchemeSources');
const boot = goog.require('r5js.boot');

/**
 * @param {!InputPort=} inputPort
 * @param {!OutputPort=} outputPort
 * @return {!AsyncEvaluatorImpl}
 */
function newAsyncEvaluator(inputPort=InputPort.NULL, outputPort=OutputPort.NULL) {
    const sources = new SchemeSources();
    const syncEvaluator = boot(sources.syntax, sources.procedures, inputPort, outputPort);
    return new AsyncEvaluatorImpl(syncEvaluator);
}

/** Wraps a synchronous evaluator in promises. */
class AsyncEvaluatorImpl {
    /** @param {!Evaluator} evaluator */
    constructor(evaluator) {
        /** @const @private */ this.evaluator_ = evaluator;
    }

    /**
     * @param {string} input Input to evaluate.
     * @return {!Promise<string>}  If evaluation succeeds,
     * this promise will be resolved with a string representation of the Scheme
     * value (as if it was serialized with the {@code write} procedure).
     * If evaluation fails, the promise will be rejected with an {@link r5js.Error}
     * explaining what went wrong.
     */
    evaluate(input) {
        try {
            return Promise.resolve(this.evaluator_.evaluate(input));
        } catch (e) {
            return Promise.reject(e);
        }
    }
}

exports = newAsyncEvaluator;
