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

goog.provide('r5js.Evaluator');



/**
 * This is the most important interface in the whole codebase,
 * the main abstraction used by clients to evaluate Scheme source code.
 * Its methods are asynchronous, returning Promises, because
 * evaluation should not block the main thread (for example, the browser UI).
 *
 * @interface
 */
r5js.Evaluator = function() {};


/**
 * @param {string} input
 * @return {!goog.Promise.<!r5js.JsonValue>}
 */
r5js.Evaluator.prototype.evaluate = function(input) {};
