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

goog.provide('r5js.ProcCallLike');



/**
 * @param {string=} opt_lastResultName
 * @struct
 * @constructor
 */
r5js.ProcCallLike = function(opt_lastResultName) {
  /** @private */ this.resultName_ = opt_lastResultName ||
      ('@' /* TODO bl document */ + goog.getUid(this));
  /** @private {r5js.ProcCallLike} */ this.next_ = null;
  /** @private {r5js.IEnvironment} */ this.env_ = null;
};


/**
 * @param {!r5js.TrampolineHelper} trampolineHelper
 * @param {!r5js.IEnvironment} env
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 */
r5js.ProcCallLike.prototype.evalAndAdvance = goog.abstractMethod;


/** @return {string} */
r5js.ProcCallLike.prototype.getResultName = function() {
  return this.resultName_;
};


/**
 * @param {string} resultName
 * TODO bl remove.
 */
r5js.ProcCallLike.prototype.setResultName = function(resultName) {
  this.resultName_ = resultName;
};


/** @param {!r5js.IEnvironment} env */
r5js.ProcCallLike.prototype.setStartingEnv = function(env) {
  this.env_ = env;
};


/** @return {r5js.IEnvironment} */
r5js.ProcCallLike.prototype.getEnv = function() {
  return this.env_;
};


/** Clears the current environment. TODO bl not well understood. */
r5js.ProcCallLike.prototype.clearEnv = function() {
  this.env_ = null;
};


/** @return {r5js.ProcCallLike} */
r5js.ProcCallLike.prototype.getNext = function() {
  return this.next_;
};


/** @param {!r5js.ProcCallLike} next */
r5js.ProcCallLike.prototype.setNext = function(next) {
  this.next_ = next;
};


/** @param {!r5js.runtime.Value} val */
r5js.ProcCallLike.prototype.bindResult = function(val) {
  /* If the next procedure call already has an environment,
     bind the result there. Otherwise, bind it in the current
     environment; it will be carried forward by the EnvBuffer. */
  var envToUse = (this.next_ && this.next_.getEnv()) || this.env_;
  envToUse.addBinding(this.resultName_, val);
};


/**
 * @param {!r5js.ProcCallLike} procCallLike
 * @return {!r5js.ProcCallLike}
 */
r5js.ProcCallLike.getLast = function(procCallLike) {
  var maybeNext = procCallLike.getNext();
  return maybeNext ? r5js.ProcCallLike.getLast(maybeNext) : procCallLike;
};


/**
 * @param {!r5js.ProcCallLike} procCallLike
 * @param {!r5js.ProcCallLike} next The next continuable.
 */
r5js.ProcCallLike.appendProcCallLike = function(procCallLike, next) {
  r5js.ProcCallLike.getLast(procCallLike).setNext(next);
};

