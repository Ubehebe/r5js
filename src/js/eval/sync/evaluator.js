goog.module('r5js.sync.Evaluator');

const Datum = goog.require('r5js.Datum');
const error = goog.require('r5js.Error');
const Evaluator = goog.require('r5js.sync.Evaluator');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const Pipeline = goog.require('r5js.Pipeline').Interface;
const valutil = goog.require('r5js.valutil');

/** @interface */
class Interface {
 /**
  * @param {string} input
  * @return {string}
  * @throws {!Error}
  */
 evaluate(input) {}
}

/** @implements {Interface} */
class Impl {
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

/**
 * @param {!Pipeline} pipeline
 * @param {!InputPort} inputPort
 * @param {!OutputPort} outputPort
 */
function create(pipeline, inputPort, outputPort) {
    return new Impl(pipeline, inputPort, outputPort);
}

exports.Interface = Interface;
exports.create = create;