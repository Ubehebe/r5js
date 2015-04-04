goog.module('r5js.TrampolineHelper');

const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const ProcCallLike = goog.require('r5js.ProcCallLike');
const UNSPECIFIED_VALUE = goog.require('r5js.runtime.UNSPECIFIED_VALUE');
const Value = goog.require('r5js.runtime.Value');

class TrampolineHelper {
    /**
     * @param {!InputPort} inputPort
     * @param {!OutputPort} outputPort
     */
    constructor(inputPort, outputPort) {
        /** @const @private */ this.inputPort_ = inputPort;
        /** @const @private */ this.outputPort_ = outputPort;
        /** @private {ProcCallLike} */ this.beforeThunk_ = null;
        /** @private {ProcCallLike} */ this.nextContinuable_ = null;
        /** @private {!Value} */
        this.value_ = r5js.runtime.UNSPECIFIED_VALUE;
    }

    /** Clears the object's state. TODO bl: not {@link beforeThunk}? */
    clear() {
        this.nextContinuable_ = null;
    }

    /** @return {ProcCallLike} */
    getBeforeThunk() {
        return this.beforeThunk_;
    }

    /** @param {ProcCallLike} beforeThunk */
    setBeforeThunk(beforeThunk) {
        this.beforeThunk_ = beforeThunk;
    }

    /** @return {ProcCallLike} */
    getNextProcCallLike() {
        return this.nextContinuable_;
    }

    /** @param {!ProcCallLike} procCallLike */
    setNext(procCallLike) {
        this.nextContinuable_ = procCallLike;
    }

    /** @return {!Value} */
    getValue() {
        return this.value_;
    }

    /** @param {!Value} value */
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