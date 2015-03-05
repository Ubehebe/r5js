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

goog.provide('r5js.Platform');



/**
 * Abstraction of the (JavaScript) platform that the Scheme implementation
 * is running in.
 * @interface
 */
r5js.Platform = function() {};


/** @param {number} statusCode */
r5js.Platform.prototype.exit = function(statusCode) {};


/**
 * @param {!r5js.InputPort=} opt_inputPort
 * @param {!r5js.OutputPort=} opt_outputPort
 * @return {!goog.Promise<!r5js.Evaluator>}
 */
r5js.Platform.prototype.newEvaluator =
    function(opt_inputPort, opt_outputPort) {};
