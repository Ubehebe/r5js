goog.module('r5js.async.Evaluator.Impl');

const InputPort = goog.require('r5js.InputPort');
const Promise = goog.require('goog.Promise');
const SchemeSources = goog.require('r5js.SchemeSources');
const {OutputPort, NULL_OUTPUT_PORT} = goog.require('r5js.OutputPort');
const {boot} = goog.require('r5js.boot');

/** Wraps a synchronous evaluator in promises. */
class AsyncEvaluator {
    /**
     * @param {!InputPort=} inputPort
     * @param {!OutputPort=} outputPort
     */
    constructor(inputPort=InputPort.NULL, outputPort=NULL_OUTPUT_PORT) {
        const sources = new SchemeSources();
        /** @const @private */ this.evaluator_ = boot(sources.syntax, sources.procedures, inputPort, outputPort);
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

exports = AsyncEvaluator;
