goog.provide('r5js.ContinuableHelper');

goog.require('r5js.ProcCallLike');

/**
 * A buffer to accumulate a Continuable-Continuation chain
 * without the caller having to do the pointer arithmetic.
 */
r5js.ContinuableHelper = class {
    constructor() {
        /** @private {r5js.ProcCallLike} */ this.firstProcCallLike_ = null;
        /** @private {r5js.ProcCallLike} */ this.lastProcCallLike_ = null;
    }

    /** @param {!r5js.ProcCallLike} procCallLike A continuable object. */
    appendProcCallLike(procCallLike) {
        if (!this.firstProcCallLike_) {
            this.firstProcCallLike_ = procCallLike;
        } else {
            this.lastProcCallLike_.setNext(procCallLike);
        }
        this.lastProcCallLike_ = r5js.ProcCallLike.getLast(procCallLike);
    }

    /** @return {r5js.ProcCallLike} */
    toContinuable() {
        return this.firstProcCallLike_;
    }
};