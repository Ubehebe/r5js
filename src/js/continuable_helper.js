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


goog.provide('r5js.tmp.continuable_helper');

/* Just a buffer to accumulate a Continuable-Continuation chain
 without the client having to do the pointer arithmetic. */
function ContinuableHelper() {
    // this.firstContinuable;
    // this.lastContinuable;
}

ContinuableHelper.prototype.appendContinuable = function(continuable) {

    if (!this.firstContinuable) {
        this.firstContinuable = continuable;
        this.lastContinuable = continuable.getLastContinuable();
    } else {
        this.lastContinuable.continuation.nextContinuable = continuable;
        this.lastContinuable = continuable.getLastContinuable();
    }
};

ContinuableHelper.prototype.toContinuable = function() {
    return this.firstContinuable;
};