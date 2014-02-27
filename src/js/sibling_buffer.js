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


goog.provide('r5js.SiblingBuffer');

/**
 * Just a buffer to accumulate siblings without the client having to do
 * the pointer arithmetic.
 * @constructor
 */
r5js.SiblingBuffer = function() {};

/**
 * @type {r5js.Datum}
 * @private
 */
r5js.SiblingBuffer.prototype.first_;

/**
 * @type {r5js.Datum}
 * @private
 */
r5js.SiblingBuffer.prototype.last_;

/**
 * @return {boolean} True iff the buffer is empty.
 */
r5js.SiblingBuffer.prototype.isEmpty = function() {
    return !this.first_;
};

/**
 * @param {!r5js.Datum} node Node to append.
 * @return {!r5js.SiblingBuffer} This object, for chaining.
 */
r5js.SiblingBuffer.prototype.appendSibling = function(node) {
    if (node) {
        if (!this.first_) {
            this.first_ = node;
            this.last_ = node.lastSibling();
        } else {
            this.last_.setNextSibling(node);
            this.last_ = node.lastSibling();
        }
    }
    return this;
};

/**
 * @return {r5js.Datum}
 */
r5js.SiblingBuffer.prototype.toSiblings = function() {
    return this.first_;
};

/**
 * @param {*=} type
 * TODO bl: narrow the type of the parameter.
 */
r5js.SiblingBuffer.prototype.toList = function(type) {
  var ans = newEmptyList();
    ans.setFirstChild(this.first_);
    if (type) {
        ans.setType(type); // default is (
    }
    return ans;
};

/** @override */
r5js.SiblingBuffer.prototype.toString = function() {
    var tmp = newEmptyList();
    tmp.appendChild(this.first_);
    return tmp.toString();
};