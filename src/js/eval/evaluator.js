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
 * Ideally, this interface should have only one method, mapping input text
 * to Scheme values: function(string):!r5js.runtime.Value. Unfortunately,
 * this implementation is limited in the extent it can serialize Scheme values.
 * The implementation represents many Scheme values with JavaScript objects,
 * which have methods, and methods cannot be serialized by the HTML5
 * structured clone algorithm that governs the interaction of web workers
 * with their parent contexts. So I have chosen to split this interface into two
 * methods, each of which serializes Scheme values in a different way.
 *
 * @interface
 */
r5js.Evaluator = function() {};


/**
 * @param {string} input
 * @return {!goog.Promise.<string>}
 */
r5js.Evaluator.prototype.evaluateToString = function(input) {};


/**
 * @param {string} input
 * @return {!goog.Promise.<?>}
 */
r5js.Evaluator.prototype.evaluateToJs = function(input) {};
