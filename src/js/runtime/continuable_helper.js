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


goog.provide('r5js.ContinuableHelper');


goog.require('r5js.ProcCallLike');



/**
 * A buffer to accumulate a Continuable-Continuation chain
 * without the caller having to do the pointer arithmetic.
 * @struct
 * @constructor
 */
r5js.ContinuableHelper = function() {
  /** @private {r5js.ProcCallLike} */ this.firstProcCallLike_ = null;
  /** @private {r5js.ProcCallLike} */ this.lastProcCallLike_ = null;
  /** @private {r5js.Continuation} */ this.firstContinuation_ = null;
};


/** @param {!r5js.ProcCallLike} procCallLike A continuable object. */
r5js.ContinuableHelper.prototype.appendProcCallLike = function(procCallLike) {
  if (!this.firstProcCallLike_) {
    this.firstProcCallLike_ = procCallLike;
    this.firstContinuation_ = procCallLike.getContinuation();
    this.lastProcCallLike_ = r5js.ProcCallLike.getLast(procCallLike);
  } else {
    this.lastProcCallLike_.
        getContinuation().
        setNextContinuable(procCallLike);
    this.lastProcCallLike_ = r5js.ProcCallLike.getLast(procCallLike);
  }
};


/** @return {!r5js.ProcCallLike} */
r5js.ContinuableHelper.prototype.toContinuable = function() {
  this.firstProcCallLike_.setContinuation(
      /** @type {!r5js.Continuation} */ (this.firstContinuation_));
  return this.firstProcCallLike_;
};
