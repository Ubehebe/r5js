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

/* todo bl: Continuable was originally envisioned as the parent
 type of objects on the trampoline. Originally we had three subtypes:
 ProcCall, Branch, and IdShim. But IdShim was turned into a subtype of
 ProcCall in order to take advantage of ProcCall's environment-handling
 logic. Subsequently other "procedure-call-like" entities (assignments,
 for example) have been written as ProcCalls. So it may no longer make
 sense to create both a Continuable and a ProcCall object for most
 things on the trampoline. */
function Continuable(subtype, continuation) {
    if (!subtype || !continuation) // todo bl take out after testing
        throw new InternalInterpreterError('invariant incorrect');
    this.subtype = subtype;
    this.continuation = continuation;
    //this.lastContinuable = this.getLastContinuable(); // todo bl caching problems
}

Continuable.prototype.setStartingEnv = function(env) {
    if (this.subtype instanceof ProcCall)
        this.subtype.setEnv(env, true);

    return this;
};

Continuable.prototype.setTopLevelAssignment = function() {
    if (!(this.subtype instanceof ProcCall
        && this.subtype.operatorName === ProcCall.prototype.specialOps._set))
        throw new InternalInterpreterError('invariant incorrect');
    this.subtype.isTopLevelAssignment = true;
    return this;
};

Continuable.prototype.setSyntaxAssignment = function() {
    if (!(this.subtype instanceof ProcCall
        && this.subtype.operatorName === ProcCall.prototype.specialOps._set))
        throw new InternalInterpreterError('invariant incorrect');
    this.subtype.isSyntaxAssignment = true;
    return this;
};

/* The last continuable of a continuable-continuation chain is the first
 continuable c such that c.continuation.nextContinuable is null. */
Continuable.prototype.getLastContinuable = function() {
    if (!this.continuation)
        throw new InternalInterpreterError('invariant violated');
    return this.continuation.nextContinuable
        ? this.continuation.nextContinuable.getLastContinuable()
        : this;
};

Continuable.prototype.appendContinuable = function(next) {
    this.getLastContinuable().continuation.nextContinuable = next;
    return this;
};

// delegate to subtype, passing in the continuation for debugging
Continuable.prototype.toString = function(indentLevel) {
    return this.subtype.toString(this.continuation, indentLevel || 0);
};