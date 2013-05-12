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


goog.provide('r5js.tmp.continuation');


goog.require('r5js.InternalInterpreterError');

/**
 * @constructor
 */
function Continuation(lastResultName) {

    this.lastResultName = lastResultName;

    /* Example: (g (f x y) z) desugared is
     (f x y [f' (g f' z [g' ...])])
     The continuation c is [f' (g f' z [g' ...])]
     c.lastResultName is f'
     c.nextContinuable is (g f' z ...)
     */
}

/* Just for call/ccs inside dynamic-winds.
 todo bl: document why we don't have to install the "after" thunk.
 (I'm pretty sure the reason is it's already in the continuable chain
 somewhere.) */
Continuation.prototype.installBeforeThunk = function(before) {
    this.beforeThunk = before;
};

// Just for debugging
Continuation.prototype.debugString = function(indentLevel) {

    if (indentLevel == null) {
        /* If no indent level is given, this function is being used to
         construct an external representation, so we should hide all the
         implementation details. It's legal to return continuations directly,
         as in

         (define x 3)
         (call-with-current-continuation (lambda (c) (set! x c)))
         x
         */
        return '[continuation]';
    } else {

        // Otherwise this is being used for debugging, show all the things.

        var ans = '[' + this.lastResultName;

        if (this.nextContinuable) {
            for (var i = 0; i < indentLevel; ++i)
                ans += '\t';
            ans += ' ' + this.nextContinuable.debugString(indentLevel + 1);
        }
        return ans + ']';
    }
};

/* This returns null if the very next Continuable isn't a ProcCall
(in particular, if it is a Branch). */
Continuation.prototype.getAdjacentProcCall = function() {
    return this.nextContinuable
        && this.nextContinuable.subtype instanceof ProcCall
        && this.nextContinuable.subtype;
};

Continuation.prototype.rememberEnv = function(env) {
    /* In general, we need to remember to jump out of the newEnv at
     the end of the procedure body. See ProcCall.prototype.maybeSetEnv
     for detailed logic (and maybe bugs). */
    if (this.nextContinuable) {
        var next = this.nextContinuable.subtype;
        if (next instanceof ProcCall)
            next.maybeSetEnv(env);
        /* Somewhat tricky. We can't know in advance which branch we'll take,
         so we set the environment on both branches. Later, when we actually
         decide which branch to take, we must clear the environment on the
         non-taken branch to prevent old environments from hanging around.

         todo bl: it would probably be better to remember the environment on
         the Branch directly. Then Branch.prototype.evalAndAdvance can set the
         environment on the taken branch without having to remember to clear
         it off the non-taken branch. I'll save this for the next time I refactor
         ProcCalls and Branches. (The explicit "subtypes" suggest my command of
         prototypal inheritance wasn't great when I wrote this code.) */
        else if (next instanceof Branch) {
            if (next.consequent.subtype instanceof ProcCall)
                next.consequent.subtype.maybeSetEnv(env);
            if (next.alternate.subtype instanceof ProcCall)
                next.alternate.subtype.maybeSetEnv(env);
        } else throw new r5js.InternalInterpreterError('invariant incorrect');
    }
};