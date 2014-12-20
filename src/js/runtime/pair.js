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

goog.provide('r5js.IPair');



/** @interface */
r5js.IPair = function() {};


/** @return {!r5js.runtime.Value} */
r5js.IPair.prototype.car = function() {};


/** @return {!r5js.runtime.Value} */
r5js.IPair.prototype.cdr = function() {};


/** @const @private */
r5js.IPair.IMPLEMENTED_BY_PROP_ = '$r5js.IPair';


/**
 * @param {*} obj
 * @return {boolean}
 */
r5js.IPair.isImplementedBy = function(obj) {
  return !!(obj && obj[r5js.IPair.IMPLEMENTED_BY_PROP_]);
};


/**
 * @param {function(new: r5js.IPair, ...)} ctor
 */
r5js.IPair.addImplementation = function(ctor) {
  ctor.prototype[r5js.IPair.IMPLEMENTED_BY_PROP_] = true;
};
