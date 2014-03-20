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


goog.provide('r5js.CdrHelper');


goog.require('r5js.ImmutableError');
goog.require('r5js.List');



/**
 * See the comment to {@link r5js.Datum.siblingsToList}
 * for an explanation of what this class does.
 * @param {!r5js.Datum} head
 * @param {!r5js.Datum} startOfCdr
 * @struct
 * @constructor
 */
r5js.CdrHelper = function(head, startOfCdr) {
  /** @const @private {!r5js.Datum} */
  this.head_ = head;

  /** @const @private {!r5js.Datum} */
  this.startOfCdr_ = startOfCdr;
};


/**
 * TODO bl: this used to return this.head_.getCdrHelper(),
 * but I am not sure that's necessary.
 * @return {null}
 */
r5js.CdrHelper.prototype.getCdrHelper = function() {
  return null;
};


/**
 * Basically, call set-car! on the master list.
 * @param {!r5js.Datum} car TODO bl.
 */
r5js.CdrHelper.prototype.setCar = function(car) {
  if (this.head_.isImmutable()) {
    throw new r5js.ImmutableError(this.head_.toString());
  }
  this.head_.getFirstChild().setNextSibling(car);
};


/**
 * Basically, call set-cdr! on the master list.
 * @param {!r5js.Datum} cdr TODO bl.
 */
r5js.CdrHelper.prototype.setCdr = function(cdr) {
  if (this.head_.isImmutable()) {
    throw new r5js.ImmutableError(this.head_.toString());
  }
  this.startOfCdr_.setNextSibling(cdr);
  if (!(cdr instanceof r5js.List)) {
    var cur = this;
    do {
      cur.head_.setType(r5js.parse.Terminals.LPAREN_DOT);
    } while (cur = cur.head_.getCdrHelper());
  }
};


/**
 * @param {!r5js.CdrHelper} cdrHelper Helper to compare against.
 * @return {boolean} True iff the two CdrHelpers point to the same list
 * and have the same offset.
 */
r5js.CdrHelper.prototype.equals = function(cdrHelper) {
  return this.head_ === cdrHelper.head_ &&
      this.startOfCdr_ === cdrHelper.startOfCdr_;
};


/**
 * @param {r5js.Datum} datum The datum to test against.
 * @return {boolean} True iff the CdrHelper points to the given list datum
 * and its offset is that list's first child.
 */
r5js.CdrHelper.prototype.resolvesTo = function(datum) {
  if (!datum) {
    return false;
  } else if (this.head_ === datum) {
    return this.startOfCdr_ === datum.getFirstChild();
  } else {
    return false;
  }
};
