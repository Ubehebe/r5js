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


goog.provide('r5js.Continuable');


goog.require('r5js.InternalInterpreterError');
// TODO bl circular dependency -- see below goog.require('r5js.ProcCall');

/**
 * TODO bl: This constructor is only called twice, once with a
 * {@link r5js.ProcCall} as subtype, the other time with a
 * {@link Branch} as subtype. Thus this class should be turned into
 * a base class or interface, with {@link r5js.ProcCall} and {@link Branch}
 * extending or implementing it. This would break the circular dependency
 * caused by the goog.require('r5js.ProcCall') commented out above.
 *
 * @param {?} subtype
 * @param {!r5js.Continuation} continuation The continuation.
 * @constructor
 */
r5js.Continuable = function(subtype, continuation) {
    if (!subtype || !continuation) // todo bl take out after testing
        throw new r5js.InternalInterpreterError('invariant incorrect');
    this.subtype = subtype;
    this.continuation = continuation;
    //this.lastContinuable = this.getLastContinuable(); // todo bl caching problems
};

/**
 * @param {!r5js.IEnvironment} env The starting environment.
 * @return {!r5js.Continuable} This object, for chaining.
 */
r5js.Continuable.prototype.setStartingEnv = function(env) {
    if (this.subtype instanceof r5js.ProcCall) {
        this.subtype.setEnv(env, true);
    }
    return this;
};


/**
 * @return {!r5js.Continuable} This object, for chaining.
 */
r5js.Continuable.prototype.setTopLevelAssignment = function() {
    if (!(this.subtype instanceof r5js.ProcCall &&
        this.subtype.operatorName === r5js.ProcCall.prototype.specialOps._set)) {
        throw new r5js.InternalInterpreterError('invariant incorrect');
    }
    this.subtype.isTopLevelAssignment = true;
    return this;
};


/**
 * @return {!r5js.Continuable} This object, for chaining.
 */
r5js.Continuable.prototype.setSyntaxAssignment = function() {
    if (!(this.subtype instanceof r5js.ProcCall &&
        this.subtype.operatorName === r5js.ProcCall.prototype.specialOps._set)) {
        throw new r5js.InternalInterpreterError('invariant incorrect');
    }
    this.subtype.isSyntaxAssignment = true;
    return this;
};

/**
 * The last continuable of a continuable-continuation chain is the first
 * continuable c such that c.continuation.nextContinuable is null.
 * @return {!r5js.Continuable}
 */
r5js.Continuable.prototype.getLastContinuable = function() {
    if (!this.continuation) {
        throw new r5js.InternalInterpreterError('invariant violated');
    }
    return this.continuation.nextContinuable
        ? this.continuation.nextContinuable.getLastContinuable()
        : this;
};

/**
 * @param {!r5js.Continuable} next The next continuable.
 * @return {!r5js.Continuable} This object, for chaining.
 */
r5js.Continuable.prototype.appendContinuable = function(next) {
    this.getLastContinuable().continuation.nextContinuable = next;
    return this;
};

/**
 * Delegates to subtype, passing in the continuation for debugging.
 */
r5js.Continuable.prototype.debugString = function(indentLevel) {
    return this.subtype.debugString(
        this.continuation,
        indentLevel || 0
    );
};