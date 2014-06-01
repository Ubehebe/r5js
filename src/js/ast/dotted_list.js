/* Copyright 2011-2014 Brendan Linn

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

goog.provide('r5js.ast.DottedList');


goog.require('r5js.CdrHelper');
goog.require('r5js.Pair');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.CompoundDatum');



/**
 * @param {r5js.Datum} firstChild
 * @implements {r5js.Pair}
 * @extends {r5js.ast.CompoundDatum}
 * @struct
 * @constructor
 */
r5js.ast.DottedList = function(firstChild) {
  goog.base(this);
  if (firstChild) {
    this.setFirstChild(firstChild);
  }
};
goog.inherits(r5js.ast.DottedList, r5js.ast.CompoundDatum);
r5js.Pair.addImplementation(r5js.ast.DottedList);


/** @override */
r5js.ast.DottedList.prototype.isImproperList = function() { return true; };


/** @override */
r5js.ast.DottedList.prototype.car = function() {
  return /** @type {!r5js.Datum} */ (this.getFirstChild());
};


/** @override */
r5js.ast.DottedList.prototype.cdr = function() {
  var startOfCdr = this.getFirstChild().getNextSibling();
  var ans;
  if (startOfCdr) {
    if (startOfCdr.getNextSibling()) {
      ans = new r5js.SiblingBuffer().
                appendSibling(startOfCdr).
                toList(r5js.ast.DottedList);
    } else {
      ans = startOfCdr;
    }
    if (ans instanceof r5js.ast.CompoundDatum) {
      ans.setCdrHelper(new r5js.CdrHelper(this, startOfCdr));
    }
    return ans;
  } else {
    return new r5js.SiblingBuffer().toList(r5js.ast.List);
  }
};
