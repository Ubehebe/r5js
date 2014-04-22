/* Copyright 2011, 2012 Brendan Linn

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


goog.require('r5js.ProcedureLike');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Macro');



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
 *
 * @param {string=} opt_lastResultName Optional name to use for the last result.
 *     If not given, a unique name will be created.
 * @implements {r5js.ProcedureLike}
 * @struct
 * @constructor
 */
r5js.Continuation = function(opt_lastResultName) {
  /** @private {string} */
  this.lastResultName_ = goog.isDef(opt_lastResultName) ?
      opt_lastResultName :
      ('@' /* TODO bl document */ + goog.getUid(this));

  /** @private {r5js.ProcCallLike} */ this.nextContinuable_ = null;
};
r5js.ProcedureLike.addImplementation(r5js.Continuation);


/** @return {string} */
r5js.Continuation.prototype.getLastResultName = function() {
  return this.lastResultName_;
};


/**
 * @param {string} name
 * TODO bl This setter doesn't make sense. lastResultName should be const.
 */
r5js.Continuation.prototype.setLastResultName = function(name) {
  this.lastResultName_ = name;
};


/** @return {r5js.ProcCallLike} */
r5js.Continuation.prototype.getNextContinuable = function() {
  return this.nextContinuable_;
};


/** @param {!r5js.ProcCallLike} continuable */
r5js.Continuation.prototype.setNextContinuable = function(continuable) {
  this.nextContinuable_ = continuable;
};


/** @param {!r5js.IEnvironment} env Environment to remember. */
r5js.Continuation.prototype.rememberEnv = function(env) {
  /* In general, we need to remember to jump out of the newEnv at
     the end of the procedure body. See ProcCall.prototype.maybeSetEnv
     for detailed logic (and maybe bugs). */
  if (this.nextContinuable_) {
    this.nextContinuable_.maybeSetEnv(env);
  }
};


/**
 * @override
 * @suppress {accessControls}
 */
r5js.Continuation.prototype.evalAndAdvance = function(
    procCall, procCallLike, trampolineHelper, parserProvider) {
  var arg = procCall.evalArgs(false)[0]; // there will only be 1 arg
  procCall.env.addBinding(this.lastResultName_, arg);
  trampolineHelper.setValue(arg);
  if (this.nextContinuable_) {
    trampolineHelper.setNextProcCallLike(this.nextContinuable_);
  }
  r5js.Continuation.repairInfiniteLoop(procCall, trampolineHelper);
};


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
r5js.Continuation.repairInfiniteLoop = function(procCall, trampolineHelper) {
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
};
