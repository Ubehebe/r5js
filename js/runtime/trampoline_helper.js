goog.module('r5js.TrampolineHelper');

const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const UNSPECIFIED_VALUE = goog.require('r5js.UNSPECIFIED_VALUE');
const {ProcCallLike, ResultStruct} = goog.require('r5js.ProcCallLike');

/** @implements {ResultStruct} */
class TrampolineHelper {
    /**
     * @param {!InputPort} inputPort
     * @param {!OutputPort} outputPort
     */
    constructor(inputPort, outputPort) {
        /** @const @private */ this.inputPort_ = inputPort;
        /** @const @private */ this.outputPort_ = outputPort;
        /** @private {?ProcCallLike} */ this.beforeThunk_ = null;
        /** @private {?ProcCallLike} */ this.nextContinuable_ = null;
        /** @private {!Value} */ this.value_ = UNSPECIFIED_VALUE;
    }

    /** Clears the object's state. TODO bl: not {@link beforeThunk}? */
    clear() {
        this.nextContinuable_ = null;
    }

    /** @return {?ProcCallLike} */
    getBeforeThunk() {
        return this.beforeThunk_;
    }

    /** @param {!ProcCallLike} beforeThunk */
    setBeforeThunk(beforeThunk) {
        this.beforeThunk_ = beforeThunk;
    }

    /** @override */
    getNextProcCallLike() {
        return this.nextContinuable_;
    }

    /** @override */
    setNext(procCallLike) {
        this.nextContinuable_ = procCallLike;
    }

    /** @return {!Value} */
    getValue() {
        return this.value_;
    }

    /** @override */
    setValue(value) {
        this.value_ = value;
    }

    /** @return {!InputPort} */
    getInputPort() {
        return this.inputPort_;
    }

    /** @return {!OutputPort} */
    getOutputPort() {
        return this.outputPort_;
    }
}

exports = TrampolineHelper;