goog.module('r5js.ContinuableHelper');

const {ProcCallLike, getLastProcCallLike} = goog.require('r5js.ProcCallLike');

/**
 * A buffer to accumulate a Continuable-Continuation chain
 * without the caller having to do the pointer arithmetic.
 */
class ContinuableHelper {
    constructor() {
        /** @private {?ProcCallLike} */ this.firstProcCallLike_ = null;
        /** @private {?ProcCallLike} */ this.lastProcCallLike_ = null;
    }

    /** @param {!ProcCallLike} procCallLike A continuable object. */
    appendProcCallLike(procCallLike) {
        if (!this.firstProcCallLike_) {
            this.firstProcCallLike_ = procCallLike;
        } else {
            this.lastProcCallLike_.setNext(procCallLike);
        }
        this.lastProcCallLike_ = getLastProcCallLike(procCallLike);
    }

    /** @return {?ProcCallLike} */
    toContinuable() {
        return this.firstProcCallLike_;
    }
}

exports = ContinuableHelper;