goog.module('r5js.DynamicWindContinuation');

const Continuation = goog.require('r5js.Continuation');
const {ProcCallLike} = goog.require('r5js.ProcCallLike');

/**
 * Just for call/ccs inside dynamic-winds.
 * TODO bl: document why we don't have to install the "after" thunk.
 * (I'm pretty sure the reason is it's already in the continuable chain
 * somewhere.)
 */
class DynamicWindContinuation extends Continuation {
    /**
     * @param {!ProcCallLike} thunk
     * @param {?ProcCallLike} nextProcCallLike
     * @param {string} lastResultName
     */
    constructor(thunk, nextProcCallLike, lastResultName) {
        super(lastResultName, nextProcCallLike);
        /** @const @private */ this.thunk_ = thunk;
    }

    /** @override */
    evaluate(arg, procCallLike, trampolineHelper) {
        procCallLike.getEnv().addBinding(this.lastResultName_, arg);
        trampolineHelper.setValue(arg);
        trampolineHelper.setNext(this.thunk_);
        if (this.nextContinuable_) {
            ProcCallLike.appendProcCallLike(this.thunk_, this.nextContinuable_);
        }
        Continuation.repairInfiniteLoop(procCallLike, trampolineHelper);
    }
}

exports = DynamicWindContinuation;
