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

goog.provide('r5js.tmp.branch');

function newBranch(testIdOrLiteral, consequentContinuable, alternateContinuable, continuation) {
    return new Continuable(
        new Branch(testIdOrLiteral, consequentContinuable, alternateContinuable),
        continuation);
}

// For composition; should only be called from newBranch
/**
 * @constructor
 */
function Branch(testIdOrLiteral, consequentContinuable, alternateContinuable) {
    this.test = testIdOrLiteral;
    this.consequent = consequentContinuable;
    /* If there's no alternate given, we create a shim that will return
     an undefined value. Example:

     (display (if #f 42))

     We give a type of "number" for the shim because passing in a null type
     would activate the default type, identifier, which would change the
     semantics. */
    this.alternate = alternateContinuable
        || newIdShim(newIdOrLiteral(null, 'number'), newCpsName());
    this.consequentLastContinuable = this.consequent.getLastContinuable();
    this.alternateLastContinuable = this.alternate.getLastContinuable();
}

Branch.prototype.evalAndAdvance = function(continuation, resultStruct, envBuffer) {

    /* Branches always use the old environment left by the previous action
    on the trampoline. */
    var testResult = this.test.isIdentifier()
        ? envBuffer.get(this.test.payload)
        : maybeWrapResult(this.test, this.test.type).payload;
    if (testResult === false) {
        this.alternateLastContinuable.continuation = continuation;
        resultStruct.nextContinuable = this.alternate;
        /* We must clear the environment off the non-taken branch.
         See comment at Continuation.prototype.rememberEnv. */
        if (this.consequent.subtype instanceof ProcCall)
            this.consequent.subtype.clearEnv();
    } else {
        this.consequentLastContinuable.continuation = continuation;
        resultStruct.nextContinuable = this.consequent;
        /* We must clear the environment off the non-taken branch.
         See comment at Continuation.prototype.rememberEnv. */
        if (this.alternate.subtype instanceof ProcCall)
            this.alternate.subtype.clearEnv();
    }

    return resultStruct;
};

// Just for debugging
Branch.prototype.toString = function(continuation, indentLevel) {

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
        + this.consequent.toString(indentLevel + 1)
        + (this.alternate && this.alternate.toString(indentLevel + 1));
    if (continuation) {
        ans += '\n';
        for (i=0; i < indentLevel; ++i)
            ans += '\t';
        ans += continuation.toString(indentLevel + 1);
    }

    this.consequentLastContinuable.continuation = tmpConsequent;
    this.alternateLastContinuable.continuation = tmpAlternate;

    return ans + '}';
};