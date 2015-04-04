goog.module('r5js.Continuation');

const ProcCallLike = goog.require('r5js.ProcCallLike');
const TrampolineHelper = goog.require('r5js.TrampolineHelper');

/**
 * Example: (g (f x y) z) desugared is
 *
 * (f x y [f' (g f' z [g' ...])])
 *
 * The continuation c is [f' (g f' z [g' ...])]
 *
 * c.lastResultName is f'
 * c.nextContinuable is (g f' z ...)
 */
class Continuation {

    /**
     * @param {string} resultName Optional name to use for the last result.
     *     If not given, a unique name will be created.
     * @param {ProcCallLike} next
     */
    constructor(resultName, next) {
        /** @const @private */ this.lastResultName_ = resultName;
        /** @const @private */ this.nextContinuable_ = next;
    }

    /**
     * @param {?} arg
     * @param {!ProcCallLike} procCallLike
     * @param {!TrampolineHelper} trampolineHelper
     */
    evaluate(arg, procCallLike, trampolineHelper) {
        procCallLike.getEnv().addBinding(this.lastResultName_, arg);
        trampolineHelper.setValue(arg);
        if (this.nextContinuable_) {
            trampolineHelper.setNext(this.nextContinuable_);
        }
        Continuation.repairInfiniteLoop(procCallLike, trampolineHelper);
    }

    /**
     * Cut out the current proc call from the continuation chain to avoid an
     * infinite loop. Example:
     *
     * (define cont #f)
     * (display
     * (call-with-current-continuation
     * (lambda (c)
     * (set! cont c)
     * "inside continuation")))
     * (cont "outside continuation")
     * 42
     *
     * This should display "inside continuation", then "outside continuation",
     * then return 42. When the trampoline is at
     *
     * (cont "outside continuation")
     *
     * proc.nextContinuable will be something like
     *
     * (cont "outside continuation" _0 [_0 (id 42 [_1 ...])])
     *
     * We clearly have to cut out the first part of this chain to avoid an
     * infinite loop.
     *
     * @param {!ProcCallLike} procCall
     * @param {!TrampolineHelper} trampolineHelper
     * @protected
     */
    static repairInfiniteLoop(procCall, trampolineHelper) {
        for (var tmp = trampolineHelper.getNextProcCallLike(), prev;
             tmp;
             prev = tmp, tmp = tmp.getNext()) {
            if (tmp === procCall) {
                if (prev) {
                    // TODO bl remove cast. At least one test relies on
                    // setNext(null) here.
                    prev.setNext(/** @type {!ProcCallLike} */ (tmp.getNext()));
                }
                return;
            }
        }
    }
}

exports = Continuation;