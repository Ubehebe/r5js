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


goog.provide('r5js.CdrHelper');


goog.require('r5js.ast.List');
goog.require('r5js.error');



/**
 * See the comment to {@link r5js.Datum.siblingsToList}
 * for an explanation of what this class does.
 * @param {!r5js.ast.CompoundDatum} head
 * @param {!r5js.Datum} startOfCdr
 * @struct
 * @constructor
 */
r5js.CdrHelper = class {
    /**
     * @param {!r5js.ast.CompoundDatum} head
     * @param {!r5js.Datum} startOfCdr
     */
    constructor(head, startOfCdr) {
        /** @const @private */ this.head_ = head;

        /** @const @private {!r5js.Datum} */
        this.startOfCdr_ = startOfCdr;
    }

    /**
     * Basically, call set-car! on the master list.
     * @param {!r5js.Datum} car TODO bl.
     */
    setCar(car) {
        if (this.head_.isImmutable()) {
            throw r5js.error.immutable(this.head_.toString());
        }
        this.head_.getFirstChild().setNextSibling(car);
    }

    /**
     * Basically, call set-cdr! on the master list.
     * @param {!r5js.Datum} cdr TODO bl.
     */
    setCdr(cdr) {
        if (this.head_.isImmutable()) {
            throw r5js.error.immutable(this.head_.toString());
        }
        this.startOfCdr_.setNextSibling(cdr);
        if (!(cdr instanceof r5js.ast.List)) {
            var cur = this;
            do {
                if (cur.head_ instanceof r5js.ast.List) {
                    cur.head_.markDotted();
                }
            } while (cur = cur.head_.getCdrHelper());
        }
    }

    /**
     * @param {!r5js.CdrHelper} cdrHelper Helper to compare against.
     * @return {boolean} True iff the two CdrHelpers point to the same list
     * and have the same offset.
     */
    equals(cdrHelper) {
        return this.head_ === cdrHelper.head_ &&
            this.startOfCdr_ === cdrHelper.startOfCdr_;
    }

    /**
     * @param {r5js.ast.CompoundDatum} datum The datum to test against.
     * @return {boolean} True iff the CdrHelper points to the given list datum
     * and its offset is that list's first child.
     */
    resolvesTo(datum) {
        if (!datum) {
            return false;
        } else if (this.head_ === datum) {
            return this.startOfCdr_ === datum.getFirstChild();
        } else {
            return false;
        }
    }
};