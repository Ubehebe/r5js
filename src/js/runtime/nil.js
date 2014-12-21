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
goog.provide('r5js.runtime.NIL');



/**
 * @implements {r5js.runtime.ObjectValue}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.Nil_ = function() {};


/**
 * @param {!r5js.runtime.Value} other
 * @return {boolean}
 */
r5js.runtime.Nil_.prototype.eqv = function(other) {
  return this === other;
};


/** @type {!r5js.runtime.Value} */
r5js.runtime.NIL = new r5js.runtime.Nil_();
