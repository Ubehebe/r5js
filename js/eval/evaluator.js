goog.module('r5js.Evaluator');

const Datum = goog.require('r5js.Datum');
const InputPort = goog.require('r5js.InputPort');
const Pipeline = goog.require('r5js.Pipeline');
const valutil = goog.require('r5js.valutil');
const {OutputPort} = require('/js/io/output_port_collect_es6_sources.es6/node_modules/__main__/js/io/output_port');

class Evaluator {
    /**
     * @param {!Pipeline} pipeline
     * @param {!InputPort} inputPort
     * @param {!OutputPort} outputPort
     */
    constructor(pipeline, inputPort, outputPort) {
        /** @const @private */ this.pipeline_ = pipeline;
        /** @const @private */ this.inputPort_ = inputPort;
        /** @const @private */ this.outputPort_ = outputPort;
    }

    /**
     * @param {string} input
     * @return {string}
     * @throws {!Error}
     */
    evaluate(input) {
        return valutil.toWriteString(
            this.pipeline_.Eval(
                this.pipeline_.desugar(
                    this.pipeline_.parse(/** @type {!Datum} */ (
                        this.pipeline_.read(
                            this.pipeline_.scan(input))))),
                this.inputPort_,
                this.outputPort_));
    }
}

exports = Evaluator;