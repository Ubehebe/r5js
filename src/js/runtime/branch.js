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

goog.provide('r5js.newBranch');


goog.require('r5js.Continuable');
goog.require('r5js.Continuation');
goog.require('r5js.ProcCall');
goog.require('r5js.ProcCallLike');


/**
 * @param {string} testResultName
 * @param {!r5js.ProcCall} consequent
 * @param {!r5js.ProcCall} alternate
 * @return {!r5js.ProcCallLike}
 * TODO bl: {@link r5js.ProcCall}s are required instead of
 * {@link r5js.ProcCallLike}s by the calls to {@link r5js.ProcCall#clearEnv} in
 * {@link r5js.Branch_#evalAndAdvance}. But it doesn't seem like branches
 * should have to start with procedure calls. Figure out the discrepancy.
 */
r5js.newBranch = function(testResultName, consequent, alternate) {
  var branch = new r5js.Branch_(testResultName, consequent, alternate);
  branch.setContinuation(new r5js.Continuation());
  return branch;
};



/**
 * @param {string} testResultName
 * @param {!r5js.ProcCall} consequent
 * @param {!r5js.ProcCall} alternate
 * @implements {r5js.ProcCallLike}
 * @struct
 * @constructor
 * @private
 */
r5js.Branch_ = function(testResultName, consequent, alternate) {
  /** @const @private */ this.testResultName_ = testResultName;
  /** @const @private */ this.consequent_ = consequent;
  /** @const @private */ this.alternate_ = alternate;
  /** @const @private */ this.consequentLastContinuable_ =
      r5js.ProcCallLike.getLast(this.consequent_);
  /** @const @private */ this.alternateLastContinuable_ =
      r5js.ProcCallLike.getLast(this.alternate_);
  /** @private {r5js.Continuation} */ this.continuation_ = null;
};


/** @override */
r5js.Branch_.prototype.getContinuation = function() {
  return this.continuation_;
};


/** @override */
r5js.Branch_.prototype.setContinuation = function(continuation) {
  this.continuation_ = continuation;
};


/** @override */
r5js.Branch_.prototype.setStartingEnv = function(env) {
  // TODO bl unify with maybeSetEnv
};


/** @override */
r5js.Branch_.prototype.evalAndAdvance = function(
    continuation, resultStruct, envBuffer, parserProvider) {

  /* Branches always use the old environment left by the previous action
    on the trampoline. */
  var testResult = envBuffer.getEnv().get(this.testResultName_);
  if (testResult === false) {
    this.alternateLastContinuable_.setContinuation(continuation);
    resultStruct.setNextProcCallLike(this.alternate_);
    /* We must clear the environment off the non-taken branch.
         See comment at {@link r5js.Continuation.rememberEnv}.
         TODO bl: clearEnv is defined only on {@link r5js.ProcCall},
         yet all of the tests pass. This suggests either test coverage
         is insufficient or that I don't understand the type of subtype. */
    this.consequent_.clearEnv();
  } else {
    this.consequentLastContinuable_.setContinuation(continuation);
    resultStruct.setNextProcCallLike(this.consequent_);
    /* We must clear the environment off the non-taken branch.
         See comment at {@link r5js.Continuation.rememberEnv}, and above. */
    this.alternate_.clearEnv();
  }
};


/**
 * Somewhat tricky. We can't know in advance which branch we'll take,
 * so we set the environment on both branches. Later, when we actually
 * decide which branch to take, we must clear the environment on the
 * non-taken branch to prevent old environments from hanging around.
 *
 * TODO bl: it would probably be better to remember the environment on
 * the Branch directly. Then Branch.prototype.evalAndAdvance can set the
 * environment on the taken branch without having to remember to clear
 * it off the non-taken branch. I'll save this for the next time
 * I refactor ProcCalls and Branches. (The explicit "subtypes" suggest
 * my command of prototypal inheritance wasn't great when I wrote
 * this code.)
 *
 * @param {!r5js.IEnvironment} env
 */
r5js.Branch_.prototype.maybeSetEnv = function(env) {
  this.consequent_.maybeSetEnv(env);
  this.alternate_.maybeSetEnv(env);
};
