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


goog.require('r5js.parse.Terminals'); // TODO bl remove
// TODO bl cyclic dependency goog.require('r5js.ast.Quote');
// TODO bl cyclic dependency goog.require('r5js.ast.Unquote');
// TODO bl cyclic dependency goog.require('r5js.ast.UnquoteSplicing');



/**
 * Just a buffer to accumulate siblings without the client having to do
 * the pointer arithmetic.
 * @struct
 * @constructor
 */
r5js.SiblingBuffer = function() {
  /** @private {r5js.Datum} */
  this.first_ = null;

  /** @private {r5js.Datum} */
  this.last_ = null;
};


/** @return {boolean} True iff the buffer is empty. */
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


/** @return {r5js.Datum} */
r5js.SiblingBuffer.prototype.toSiblings = function() {
  return this.first_;
};


/**
 * @param {!r5js.parse.Terminal} type Type of the returned list.
 * @return {!r5js.Datum}
 */
r5js.SiblingBuffer.prototype.toList = function(type) {
  var ans;
  switch (type) {
    case r5js.parse.Terminals.BACKTICK:
      ans = new r5js.Quasiquote(this.first_);
      break;
    case r5js.parse.Terminals.COMMA:
      ans = new r5js.ast.Unquote(this.first_);
      break;
    case r5js.parse.Terminals.COMMA_AT:
      ans = new r5js.ast.UnquoteSplicing(this.first_);
      break;
    case r5js.parse.Terminals.LPAREN:
      ans = new r5js.List(this.first_);
      break;
    case r5js.parse.Terminals.LPAREN_DOT:
      ans = new r5js.DottedList(this.first_);
      break;
    case r5js.parse.Terminals.LPAREN_VECTOR:
      ans = new r5js.ast.Vector(/** @type {!r5js.Datum} */ (this.first_));
      break;
    case r5js.parse.Terminals.TICK:
      ans = new r5js.ast.Quote(this.first_);
      break;
  }
  if (this.last_ && ans) {
    this.last_.setParent(ans);
  }
  return /** @type {!r5js.Datum} */ (ans);
};
