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


goog.provide('r5js.ContinuableHelper');

/**
 * A buffer to accumulate a Continuable-Continuation chain
 * without the caller having to do the pointer arithmetic.
 * @constructor
 */
r5js.ContinuableHelper = function() {};


/**
 * @type {Continuable}
 * @private
 */
r5js.ContinuableHelper.prototype.firstContinuable_;


/**
 * @type {Continuable}
 * @private
 */
r5js.ContinuableHelper.prototype.lastContinuable_;


/**
 * @param {!Continuable} continuable A continuable object.
 */
r5js.ContinuableHelper.prototype.appendContinuable = function(continuable) {

    if (!this.firstContinuable_) {
        this.firstContinuable_ = continuable;
        this.lastContinuable_ = continuable.getLastContinuable();
    } else {
        this.lastContinuable_.continuation.nextContinuable = continuable;
        this.lastContinuable_ = continuable.getLastContinuable();
    }
};

/**
 * @return {Continuable}
 */
r5js.ContinuableHelper.prototype.toContinuable = function() {
    return this.firstContinuable_;
};