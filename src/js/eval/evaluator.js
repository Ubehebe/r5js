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



/** @interface */
r5js.Evaluator = function() {};


/**
 * @param {string} input
 * @return {!r5js.runtime.Value}
 */
r5js.Evaluator.prototype.evaluate = function(input) {};


/**
 * The main difference between an evaluator and a read-eval-print loop
 * (REPL) is that a REPL has to deal with user input that is possibly
 * incomplete, for example when a user types half of an expression on one line,
 * presses enter, and completes the expression on the next line.
 * This method arguably belongs in {@link r5js.Repl}, but keeping it here
 * exposes less of the evaluator's guts.
 * @param {string} input
 * @return {boolean} Whether input parses successfully.
 */
r5js.Evaluator.prototype.willParse = function(input) {};


/**
 * @param {!r5js.InputPort} inputPort
 * @param {!r5js.OutputPort} outputPort
 * @return {!r5js.Evaluator} A new evaluator connected to the given ports.
 */
r5js.Evaluator.prototype.withPorts = function(inputPort, outputPort) {};
