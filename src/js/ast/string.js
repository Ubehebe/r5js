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

goog.provide('r5js.ast.String');


goog.require('r5js.ast.SimpleDatum');



/**
 * @param {string} s
 * @extends {r5js.ast.SimpleDatum<string>}
 * @struct
 * @constructor
 */
r5js.ast.String = function(s) {
  goog.base(this, s);
};
goog.inherits(r5js.ast.String, r5js.ast.SimpleDatum);


/**
 * Unlike other simple datums, strings have reference equality semantics.
 * @see R5RS 6.1
 * @override
 */
r5js.ast.String.prototype.eqv = function(other) {
  return this === other;
};


/**
 * Datums representing strings have payloads of type string.
 * If they all unwrapped as JavaScript strings, it would be impossible
 * to re-wrap them correctly (noninjective mapping). We choose to store
 * identifiers unwrapped because they're expected to be more common than
 * strings.
 *
 * @override
 */
r5js.ast.String.prototype.unwrap = function() {
  return this;
};
