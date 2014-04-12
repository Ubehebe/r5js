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
goog.provide('r5js.Branch');


goog.require('r5js.Continuable');
goog.require('r5js.ProcCall');


/**
 * @param {string} testResultName
 * @param {!r5js.Continuable} consequent
 * @param {!r5js.Continuable} alternate
 * @param {!r5js.Continuation} continuation
 * @return {!r5js.Continuable}
 */
r5js.newBranch = function(testResultName, consequent, alternate, continuation) {
    return new r5js.Continuable(
        new r5js.Branch(testResultName, consequent, alternate),
        continuation);
};


/**
 * @param {string} testResultName
 * @param {!r5js.Continuable} consequent
 * @param {!r5js.Continuable} alternate
 * @constructor
 * @suppress {checkTypes} for the null argument to r5js.ast.Number ctor.
 */
r5js.Branch = function(testResultName, consequent, alternate) {
    /** @const @private */ this.testResultName_ = testResultName;
    /** @const @private */ this.consequent_ = consequent;
    /** @const @private */ this.alternate_ = alternate;
    /** @const @private */
    this.consequentLastContinuable_ = this.consequent_.getLastContinuable();
    /** @const @private */
    this.alternateLastContinuable_ = this.alternate_.getLastContinuable();
};

/**
 * @param {!r5js.Continuation} continuation
 * @param {!r5js.TrampolineHelper} resultStruct
 * @param {!r5js.EnvBuffer} envBuffer
 * @returns {*}
 */
r5js.Branch.prototype.evalAndAdvance = function(
    continuation, resultStruct, envBuffer) {

    /* Branches always use the old environment left by the previous action
    on the trampoline. */
    var testResult = envBuffer.get(this.testResultName_);
    if (testResult === false) {
        this.alternateLastContinuable_.continuation = continuation;
        resultStruct.nextContinuable = this.alternate_;
        /* We must clear the environment off the non-taken branch.
         See comment at {@link r5js.Continuation.rememberEnv}.
         TODO bl: clearEnv is defined only on {@link r5js.ProcCall},
         yet all of the tests pass. This suggests either test coverage
         is insufficient or that I don't understand the type of subtype. */
        this.consequent_.subtype.clearEnv();
    } else {
        this.consequentLastContinuable_.continuation = continuation;
        resultStruct.nextContinuable = this.consequent_;
        /* We must clear the environment off the non-taken branch.
         See comment at {@link r5js.Continuation.rememberEnv}, and above. */
        this.alternate_.subtype.clearEnv();
    }

    return resultStruct;
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
r5js.Branch.prototype.maybeSetEnv = function(env) {
    if (this.consequent_.subtype instanceof r5js.ProcCall) {
        this.consequent_.subtype.maybeSetEnv(env);
    }
    if (this.alternate_.subtype instanceof r5js.ProcCall) {
        this.alternate_.subtype.maybeSetEnv(env);
    }
};