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

goog.provide('r5js.DynamicWindContinuation');


goog.require('r5js.Continuation');
goog.require('r5js.ProcCallLike');



/**
 * Just for call/ccs inside dynamic-winds.
 * TODO bl: document why we don't have to install the "after" thunk.
 * (I'm pretty sure the reason is it's already in the continuable chain
 * somewhere.)
 * @param {!r5js.ProcCallLike} thunk
 * @param {r5js.ProcCallLike} nextProcCallLike
 * @param {string} lastResultName
 * @extends {r5js.Continuation}
 * @struct
 * @constructor
 */
r5js.DynamicWindContinuation = function(
    thunk, nextProcCallLike, lastResultName) {
  goog.base(this, lastResultName, nextProcCallLike);
  /** @const @private */ this.thunk_ = thunk;
};
goog.inherits(r5js.DynamicWindContinuation, r5js.Continuation);


/**
 * @override
 * @suppress {accessControls} TODO bl fix
 */
r5js.DynamicWindContinuation.prototype.evaluate = function(
    arg, procCallLike, trampolineHelper) {
  procCallLike.getEnv().addBinding(this.lastResultName_, arg);
  trampolineHelper.setValue(arg);
  trampolineHelper.setNext(this.thunk_);
  if (this.nextContinuable_) {
    r5js.ProcCallLike.appendProcCallLike(this.thunk_, this.nextContinuable_);
  }
  r5js.Continuation.repairInfiniteLoop(procCallLike, trampolineHelper);
};
