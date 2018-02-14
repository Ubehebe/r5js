goog.module('r5js.ProcCallResult');

const {ProcCallLike} = goog.require('r5js.ProcCallLike');

/** @interface */
class ProcCallResult {
    /** @param {!ProcCallLike} procCallLike */
    setNext(procCallLike) {
    }

    /** @param {!Value} value */
    setValue(value) {
    }

    /** @return {?ProcCallLike} */
    getNextProcCallLike() {
    }
}

exports = {ProcCallResult};
