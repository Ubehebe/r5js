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
// TODO bl circular dependency goog.require('r5js.Assignment');

/**
 * TODO bl: This constructor is only called twice, once with a
 * {@link r5js.ProcCall} as subtype, the other time with a
 * {@link r5js.Branch_} as subtype. Thus this class should be turned into
 * a base class or interface, with {@link r5js.ProcCall}
 * and {@link r5js.Branch_} extending or implementing it.
 * This would break the circular dependency caused by the
 * goog.require('r5js.ProcCall') commented out above.
 *
 * @param {!r5js.Branch_|!r5js.ProcCall} subtype
 * @param {!r5js.Continuation} continuation The continuation.
 * @implements {r5js.runtime.ObjectValue} TODO bl not appropriate?
 * @struct
 * @constructor
 */
r5js.Continuable = function(subtype, continuation) {
    /** @const @private {!r5js.Branch_|!r5js.ProcCall} */
    this.subtype_ = subtype;

    /** @private {!r5js.Continuation} */ this.continuation_ = continuation;

    // todo bl caching problems
    /** @private {r5js.Continuable} */ this.lastContinuable_ = null;
};


/** @return {!r5js.Branch_|!r5js.ProcCall} */
r5js.Continuable.prototype.getSubtype = function() {
    return this.subtype_;
};


/** @return {!r5js.Continuation} */
r5js.Continuable.prototype.getContinuation = function() {
    return this.continuation_;
};


/** @param {!r5js.Continuation} continuation */
r5js.Continuable.prototype.setContinuation = function(continuation) {
    this.continuation_ = continuation;
};

/**
 * @param {!r5js.IEnvironment} env The starting environment.
 * @return {!r5js.Continuable} This object, for chaining.
 */
r5js.Continuable.prototype.setStartingEnv = function(env) {
    if (this.subtype_ instanceof r5js.ProcCall) {
        this.subtype_.setEnv(env, true);
    }
    return this;
};


/**
 * @return {!r5js.Continuable} This object, for chaining.
 */
r5js.Continuable.prototype.setTopLevelAssignment = function() {
    if (!(this.subtype_ instanceof r5js.Assignment)) {
        throw new r5js.InternalInterpreterError('invariant incorrect');
    }
    this.subtype_.isTopLevelAssignment = true;
    return this;
};


/**
 * @return {!r5js.Continuable} This object, for chaining.
 */
r5js.Continuable.prototype.setSyntaxAssignment = function() {
    if (!(this.subtype_ instanceof r5js.Assignment)) {
        throw new r5js.InternalInterpreterError('invariant incorrect');
    }
    this.subtype_.isSyntaxAssignment = true;
    return this;
};

/**
 * The last continuable of a continuable-continuation chain is the first
 * continuable c such that c.continuation.nextContinuable is null.
 * @return {!r5js.Continuable}
 */
r5js.Continuable.prototype.getLastContinuable = function() {
    if (!this.continuation_) {
        throw new r5js.InternalInterpreterError('invariant violated');
    }
    return this.continuation_.nextContinuable
        ? this.continuation_.nextContinuable.getLastContinuable()
        : this;
};

/**
 * @param {!r5js.Continuable} next The next continuable.
 * @return {!r5js.Continuable} This object, for chaining.
 */
r5js.Continuable.prototype.appendContinuable = function(next) {
    this.getLastContinuable().continuation_.nextContinuable = next;
    return this;
};