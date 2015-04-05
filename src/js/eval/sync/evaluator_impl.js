goog.module('r5js.sync.EvaluatorImpl');

const Datum = goog.require('r5js.Datum');
const Evaluator = goog.require('r5js.sync.Evaluator');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const Pipeline = goog.require('r5js.Pipeline');
const valutil = goog.require('r5js.valutil');

/** @implements {Evaluator} */
class EvaluatorImpl {
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

    /** @override */
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

exports = EvaluatorImpl;
