goog.provide('r5js.TrampolineHelper');

goog.require('r5js.runtime.UNSPECIFIED_VALUE');



r5js.TrampolineHelper = class {
    /**
     * @param {!r5js.InputPort} inputPort
     * @param {!r5js.OutputPort} outputPort
     */
    constructor(inputPort, outputPort) {
        /** @const @private */ this.inputPort_ = inputPort;
        /** @const @private */ this.outputPort_ = outputPort;
        /** @private {r5js.ProcCallLike} */ this.beforeThunk_ = null;
        /** @private {r5js.ProcCallLike} */ this.nextContinuable_ = null;
        /** @private {!r5js.runtime.Value} */
        this.value_ = r5js.runtime.UNSPECIFIED_VALUE;
    }

    /** Clears the object's state. TODO bl: not {@link beforeThunk}? */
    clear() {
        this.nextContinuable_ = null;
    }

    /** @return {r5js.ProcCallLike} */
    getBeforeThunk() {
        return this.beforeThunk_;
    }

    /** @param {r5js.ProcCallLike} beforeThunk */
    setBeforeThunk(beforeThunk) {
        this.beforeThunk_ = beforeThunk;
    }

    /** @return {r5js.ProcCallLike} */
    getNextProcCallLike() {
        return this.nextContinuable_;
    }

    /** @param {!r5js.ProcCallLike} procCallLike */
    setNext(procCallLike) {
        this.nextContinuable_ = procCallLike;
    }

    /** @return {!r5js.runtime.Value} */
    getValue() {
        return this.value_;
    }

    /** @param {!r5js.runtime.Value} value */
    setValue(value) {
        this.value_ = value;
    }

    /** @return {!r5js.InputPort} */
    getInputPort() {
        return this.inputPort_;
    }

    /** @return {!r5js.OutputPort} */
    getOutputPort() {
        return this.outputPort_;
    }
};