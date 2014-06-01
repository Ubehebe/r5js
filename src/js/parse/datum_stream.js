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

goog.provide('r5js.DatumStream');



/** @interface */
r5js.DatumStream = function() {};


/** @return {r5js.Datum} The next datum, or null if there is none. */
r5js.DatumStream.prototype.getNextDatum = function() {};


/** @param {!r5js.Datum} next */
r5js.DatumStream.prototype.advanceTo = function(next) {};


/** Advances to child. */
r5js.DatumStream.prototype.advanceToChild = function() {};


/** Advances to sibling. */
r5js.DatumStream.prototype.advanceToNextSibling = function() {};


/** @return {boolean} True iff the advance worked. */
r5js.DatumStream.prototype.maybeAdvanceToNextSiblingOfParent = function() {};
