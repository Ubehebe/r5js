/* Copyright 2011-2014 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */


goog.provide('r5js.Continuation');



/**
 * Example: (g (f x y) z) desugared is
 *
 * (f x y [f' (g f' z [g' ...])])
 *
 * The continuation c is [f' (g f' z [g' ...])]
 *
 * c.lastResultName is f'
 * c.nextContinuable is (g f' z ...)
 *
 * @struct
 * @constructor
 */
r5js.Continuation = class {

    /**
     * @param {string} resultName Optional name to use for the last result.
     *     If not given, a unique name will be created.
     * @param {r5js.ProcCallLike} next
     */
    constructor(resultName, next) {
        /** @const @private */ this.lastResultName_ = resultName;
        /** @const @private */ this.nextContinuable_ = next;
    }

    /**
     * @param {?} arg
     * @param {!r5js.ProcCallLike} procCallLike
     * @param {!r5js.TrampolineHelper} trampolineHelper
     */
    evaluate(arg, procCallLike, trampolineHelper) {
        procCallLike.getEnv().addBinding(this.lastResultName_, arg);
        trampolineHelper.setValue(arg);
        if (this.nextContinuable_) {
            trampolineHelper.setNext(this.nextContinuable_);
        }
        r5js.Continuation.repairInfiniteLoop(procCallLike, trampolineHelper);
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
     * @param {!r5js.ProcCallLike} procCall
     * @param {!r5js.TrampolineHelper} trampolineHelper
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
                    prev.setNext(/** @type {!r5js.ProcCallLike} */ (tmp.getNext()));
                }
                return;
            }
        }
    }
};