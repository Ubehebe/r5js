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

goog.provide('r5js.Branch');


goog.require('goog.functions');
goog.require('r5js.ProcCall');
goog.require('r5js.ProcCallLike');



/**
 * @param {string} testResultName
 * @param {!r5js.ProcCall} consequent
 * @param {!r5js.ProcCall} alternate
 * @implements {r5js.ProcCallLike}
 * @struct
 * @constructor
 */
r5js.Branch = function(testResultName, consequent, alternate) {
  /** @const @private */ this.testResultName_ = testResultName;
  /** @const @private */ this.consequent_ = consequent;
  /** @const @private */ this.alternate_ = alternate;
  /** @const @private */ this.consequentLastContinuable_ =
      r5js.ProcCallLike.getLast(this.consequent_);
  /** @const @private */ this.alternateLastContinuable_ =
      r5js.ProcCallLike.getLast(this.alternate_);

  /** @private */
  this.resultName_ = '@' /* TODO bl document */ + goog.getUid(this);

  /** @private {r5js.ProcCallLike} */ this.next_ = null;
};


/** @override */
r5js.Branch.prototype.getResultName = function() {
  return this.resultName_;
};


/** @override */
r5js.Branch.prototype.setResultName = function(resultName) {
  this.resultName_ = resultName;
};


/** @override */
r5js.Branch.prototype.getNext = function() {
  return this.next_;
};


/** @override */
r5js.Branch.prototype.setNext = function(next) {
  this.next_ = next;
};


/** @override */
r5js.Branch.prototype.setStartingEnv = function(env) {
  if (!this.consequent_.getEnv()) {
    this.consequent_.setStartingEnv(env);
  }
  if (!this.alternate_.getEnv()) {
    this.alternate_.setStartingEnv(env);
  }
};


/** @override */
r5js.Branch.prototype.getEnv = goog.functions.NULL;


/**
 * @override
 * TODO bl: this method relies on the fact that this.next_ can be null.
 * The casts are incorrect. Investigate and correct.
 */
r5js.Branch.prototype.evalAndAdvance = function(
    resultStruct, envBuffer, parserProvider) {

  /* Branches always use the old environment left by the previous action
    on the trampoline. */
  var testResult = envBuffer.getEnv().get(this.testResultName_);
  if (testResult === false) {
    this.alternateLastContinuable_.setNext(
        /** @type {!r5js.ProcCallLike} */ (this.next_));
    this.alternateLastContinuable_.setResultName(this.resultName_);
    resultStruct.setNext(this.alternate_);
    /* We must clear the environment off the non-taken branch.
         See comment at {@link r5js.Continuation.rememberEnv}.
         TODO bl: clearEnv is defined only on {@link r5js.ProcCall},
         yet all of the tests pass. This suggests either test coverage
         is insufficient or that I don't understand the type of subtype. */
    this.consequent_.clearEnv();
  } else {
    this.consequentLastContinuable_.setNext(
        /** @type {!r5js.ProcCallLike} */ (this.next_));
    this.consequentLastContinuable_.setResultName(this.resultName_);
    resultStruct.setNext(this.consequent_);
    /* We must clear the environment off the non-taken branch.
         See comment at {@link r5js.Continuation.rememberEnv}, and above. */
    this.alternate_.clearEnv();
  }
};