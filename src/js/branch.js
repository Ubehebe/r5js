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


goog.require('r5js.Continuable');
goog.require('r5js.data');


function newBranch(testIdOrLiteral, consequentContinuable, alternateContinuable, continuation) {
    return new r5js.Continuable(
        new r5js.Branch(
            testIdOrLiteral,
            consequentContinuable,
            alternateContinuable
        ),
        continuation
    );
}


/**
 * @constructor
 * @private
 */
r5js.Branch = function(
    testIdOrLiteral, consequentContinuable, alternateContinuable) {
    this.test = testIdOrLiteral;
    this.consequent = consequentContinuable;
    /* If there's no alternate given, we create a shim that will return
     an undefined value. Example:

     (display (if #f 42))

     We give a type of "number" for the shim because passing in a null type
     would activate the default type, identifier, which would change the
     semantics. */
    this.alternate = alternateContinuable
        || newIdShim(r5js.data.newIdOrLiteral(null, 'number'), newCpsName());
    this.consequentLastContinuable = this.consequent.getLastContinuable();
    this.alternateLastContinuable = this.alternate.getLastContinuable();
};

/**
 * @param {!r5js.Continuation} continuation
 * @param {!r5js.TrampolineHelper} resultStruct
 * @param {!r5js.EnvBuffer} envBuffer
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser ready to parse the given datum.
 * @returns {*}
 */
r5js.Branch.prototype.evalAndAdvance = function(
    continuation, resultStruct, envBuffer, parserProvider) {

    /* Branches always use the old environment left by the previous action
    on the trampoline. */
    var testResult = this.test.isIdentifier()
        ? envBuffer.get(this.test.payload)
        : r5js.data.maybeWrapResult(this.test, this.test.type).payload;
    if (testResult === false) {
        this.alternateLastContinuable.continuation = continuation;
        resultStruct.nextContinuable = this.alternate;
        /* We must clear the environment off the non-taken branch.
         See comment at {@link r5js.Continuation.rememberEnv}.
         TODO bl: clearEnv is defined only on {@link r5js.ProcCall},
         yet all of the tests pass. This suggests either test coverage
         is insufficient or that I don't understand the type of subtype. */
        this.consequent.subtype.clearEnv();
    } else {
        this.consequentLastContinuable.continuation = continuation;
        resultStruct.nextContinuable = this.consequent;
        /* We must clear the environment off the non-taken branch.
         See comment at {@link r5js.Continuation.rememberEnv}, and above. */
        this.alternate.subtype.clearEnv();
    }

    return resultStruct;
};

// Just for debugging
r5js.Branch.prototype.debugString = function(continuation, indentLevel) {

    /* Don't print the continuations at the end of the branches;
     these are probably old and will be overwritten on the next
     this.evalAndAdvance, so they produce misleading debugging info. */

    var tmpConsequent = this.consequentLastContinuable.continuation;
    var tmpAlternate = this.alternateLastContinuable.continuation;

    this.consequentLastContinuable.continuation = null;
    this.alternateLastContinuable.continuation = null;

    var ans = '\n';
    for (var i = 0; i < indentLevel; ++i)
        ans += '\t';
    ans += '{' + this.test
        + ' ? '
        + this.consequent.debugString(indentLevel + 1)
        + (this.alternate && this.alternate.debugString(indentLevel + 1));
    if (continuation) {
        ans += '\n';
        for (i=0; i < indentLevel; ++i)
            ans += '\t';
        ans += continuation.debugString(indentLevel + 1);
    }

    this.consequentLastContinuable.continuation = tmpConsequent;
    this.alternateLastContinuable.continuation = tmpAlternate;

    return ans + '}';
};