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

goog.provide('r5js.CallWithCurrentContinuation');


goog.require('r5js.ProcCall');



/**
 * @param {!r5js.ast.Identifier} operatorName
 * @param {!r5js.Continuation} continuation
 * @extends {r5js.ProcCall}
 * @struct
 * @constructor
 */
r5js.CallWithCurrentContinuation = function(operatorName, continuation) {
  goog.base(this, operatorName, null /* firstOperand */);

  /** @const @private */ this.continuation_ = continuation;
};
goog.inherits(r5js.CallWithCurrentContinuation, r5js.ProcCall);


/** @override */
r5js.CallWithCurrentContinuation.prototype.evalArgs = function() {
  return [this.continuation_];
};
