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


goog.provide('r5js.tmp.cdr_helper');

/* See the comment to Datum.prototype.siblingsToList for an explanation
 of what this class does. */
function CdrHelper(head, startOfCdr) {
    this.head = head;
    this.startOfCdr = startOfCdr;
}

CdrHelper.prototype.getCdrHelper = function() {
    /* todo bl: this used to return this.head.getCdrHelper(), but
     I am not sure that's necessary. */
    return null;
};

// Basically, call set-car! on the master list.
CdrHelper.prototype.setCar = function(car) {
    if (this.head.isImmutable())
        throw new ImmutableError(this.head.toString());
    this.head.firstChild.nextSibling = car;
};

// Basically, call set-cdr! on the master list.
CdrHelper.prototype.setCdr = function(cdr) {
    if (this.head.isImmutable())
        throw new ImmutableError(this.head.toString());
    this.startOfCdr.nextSibling = cdr;
    if (!cdr.isList()) {
        var cur = this;
        do {
            cur.head.type = '.(';
        } while (cur = cur.head.getCdrHelper());
    }
};

/* Two CdrHelpers are equal iff they point to the same list and have
 the same offset. */
CdrHelper.prototype.equals = function(cdrHelper) {
    return this.head === cdrHelper.head
        && this.startOfCdr === cdrHelper.startOfCdr;
};

/* A CdrHelper resolves to a given Datum iff it points to that list
and its offset is that list's first child. */
CdrHelper.prototype.resolvesTo = function(datum) {
    if (!datum)
        return false;
    else if (this.head === datum)
        return this.startOfCdr === datum.firstChild;
    else
        return false;
};



