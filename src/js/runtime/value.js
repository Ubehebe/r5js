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

goog.provide('r5js.runtime.EOF');
goog.provide('r5js.runtime.ObjectValue');
goog.provide('r5js.runtime.PrimitiveValue');
goog.provide('r5js.runtime.UNSPECIFIED_VALUE');
goog.provide('r5js.runtime.Value');


/**
 * Typedef for Scheme values that this implementation represents with
 * primitive JavaScript values.
 * @typedef {boolean|number|string}
 */
r5js.runtime.PrimitiveValue;



/**
 * Interface for Scheme values that this implementation represents with
 * JavaScript objects.
 * @interface
 */
r5js.runtime.ObjectValue = function() {};


/**
 * Top-level typedef for all Scheme values.
 * @typedef {!r5js.runtime.PrimitiveValue|!r5js.runtime.ObjectValue}
 */
r5js.runtime.Value;




