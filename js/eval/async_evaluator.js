goog.module('r5js.async.Evaluator.Impl');

const {InputPort, NULL_INPUT_PORT} = require('/js/io/io_collect_es6_sources.es6/node_modules/__main__/js/io/input_port');
const Promise = goog.require('goog.Promise');
const {SchemeSources} = require('/js/scheme_sources_collect_es6_sources.es6/node_modules/__main__/js/scheme_sources');
const {OutputPort, NULL_OUTPUT_PORT} = require('/js/io/io_collect_es6_sources.es6/node_modules/__main__/js/io/output_port');
const {boot} = require('/js/eval/shim_collect_es6_sources.es6/node_modules/__main__/js/eval/boot');

/** Wraps a synchronous evaluator in promises. */
class AsyncEvaluator {
    /**
     * @param {!InputPort=} inputPort
     * @param {!OutputPort=} outputPort
     */
    constructor(inputPort=NULL_INPUT_PORT, outputPort=NULL_OUTPUT_PORT) {
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
