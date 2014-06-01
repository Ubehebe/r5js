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


goog.provide('r5js.TrampolineHelper');

goog.require('r5js.runtime.UNSPECIFIED_VALUE');



/**
 * @param {!r5js.InputPort} inputPort
 * @param {!r5js.OutputPort} outputPort
 * @struct
 * @constructor
 */
r5js.TrampolineHelper = function(inputPort, outputPort) {
  /** @const @private */ this.inputPort_ = inputPort;
  /** @const @private */ this.outputPort_ = outputPort;
  /** @private {r5js.ProcCallLike} */ this.beforeThunk_ = null;
  /** @private {r5js.ProcCallLike} */ this.nextContinuable_ = null;
  /** @private {!r5js.runtime.Value} */
  this.value_ = r5js.runtime.UNSPECIFIED_VALUE;
};


/** Clears the object's state. TODO bl: not {@link beforeThunk}? */
r5js.TrampolineHelper.prototype.clear = function() {
  this.nextContinuable_ = null;
};


/** @return {r5js.ProcCallLike} */
r5js.TrampolineHelper.prototype.getBeforeThunk = function() {
  return this.beforeThunk_;
};


/** @param {r5js.ProcCallLike} beforeThunk */
r5js.TrampolineHelper.prototype.setBeforeThunk = function(beforeThunk) {
  this.beforeThunk_ = beforeThunk;
};


/** @return {r5js.ProcCallLike} */
r5js.TrampolineHelper.prototype.getNextProcCallLike = function() {
  return this.nextContinuable_;
};


/** @param {!r5js.ProcCallLike} procCallLike */
r5js.TrampolineHelper.prototype.setNext = function(procCallLike) {
  this.nextContinuable_ = procCallLike;
};


/** @return {!r5js.runtime.Value} */
r5js.TrampolineHelper.prototype.getValue = function() {
  return this.value_;
};


/** @param {!r5js.runtime.Value} value */
r5js.TrampolineHelper.prototype.setValue = function(value) {
  this.value_ = value;
};


/** @return {!r5js.InputPort} */
r5js.TrampolineHelper.prototype.getInputPort = function() {
  return this.inputPort_;
};


/** @return {!r5js.OutputPort} */
r5js.TrampolineHelper.prototype.getOutputPort = function() {
  return this.outputPort_;
};
