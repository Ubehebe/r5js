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

goog.provide('r5js.ast.List');


goog.require('r5js.IPair');
// TODO bl circular dependency goog.require('r5js.ast.DottedList');
// TODO bl circular dependency goog.require('r5js.CdrHelper');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.CompoundDatum');



/**
 * @param {r5js.Datum} firstChild
 * @implements {r5js.IPair}
 * @extends {r5js.ast.CompoundDatum}
 * @struct
 * @constructor
 */
r5js.ast.List = function(firstChild) {
  r5js.ast.List.base(this, 'constructor');
  if (firstChild) {
    this.setFirstChild(firstChild);
  }

  /** @private */ this.dirty_ = false;

  /** @private */ this.dotted_ = false;
};
goog.inherits(r5js.ast.List, r5js.ast.CompoundDatum);
r5js.IPair.addImplementation(r5js.ast.List);


/** Marks dirty. */
r5js.ast.List.prototype.markDirty = function() {
  this.dirty_ = true;
};


/** Marks dotted. */
r5js.ast.List.prototype.markDotted = function() {
  this.dotted_ = true;
};


/** @override */
r5js.ast.List.prototype.isImproperList = function() {
  return this.dotted_;
};


/** @override */
r5js.ast.List.prototype.eqv = function(other) {
  if (this.dotted_) {
    return this === other;
  }

  if (!(other instanceof r5js.ast.CompoundDatum)) {
    return false;
  }

  if (this === other ||
      (other instanceof r5js.ast.List &&
      !this.getFirstChild() &&
      !other.getFirstChild())) {
    return true;
  }

  const thisHelper = this.getCdrHelper();
  let otherHelper = other.getCdrHelper();
  if (thisHelper && otherHelper) {
    return thisHelper.equals(otherHelper);
  } else if (thisHelper && other instanceof r5js.ast.CompoundDatum) {
    return thisHelper.resolvesTo(other);
  } else if (otherHelper) {
    return otherHelper.resolvesTo(this);
  } else {
    return false;
  }
};


/** @override */
r5js.ast.List.prototype.car = function() {
  return /** @type {!r5js.Datum} */ (this.getFirstChild());
};


/**
 * @override
 * @suppress {checkTypes} for setNextSibling(null).
 */
r5js.ast.List.prototype.cdr = function() {
    const startOfCdr = this.getFirstChild().getNextSibling();
    if (!startOfCdr) {
        return new r5js.SiblingBuffer().toList(r5js.ast.List);
    }

    if (startOfCdr.getNextSibling() || !this.dirty_) {
        // TODO bl investigate why this is happening
        if (startOfCdr.getNextSibling() === startOfCdr) {
            startOfCdr.setNextSibling(null);
        }
        const ans = new r5js.SiblingBuffer()
            .appendSibling(startOfCdr)
            .toList(this.dotted_ ? r5js.ast.DottedList : r5js.ast.List);
        ans.setCdrHelper(new r5js.CdrHelper(this, startOfCdr));
        return ans;
    } else {
        return startOfCdr;
    }
};
