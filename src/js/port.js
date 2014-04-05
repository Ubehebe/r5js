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


goog.provide('r5js.Port');



/**
 * @interface
 * @see R5RS 6.6.1-2
 */
r5js.Port = function() {};


/** @see R5RS 6.6.1 */
r5js.Port.prototype.close = function() {};



/**
 * @interface
 * @extends {r5js.Port}
 */
r5js.InputPort = function() {};


/** @return {boolean} */
r5js.InputPort.prototype.isCharReady = function() {};


/** @return {?string} */
r5js.InputPort.prototype.peekChar = function() {};


/** @return {?string} */
r5js.InputPort.prototype.readChar = function() {};



/**
 * @interface
 * @extends {r5js.Port}
 */
r5js.OutputPort = function() {};


/** @param {string} str String to write. */
r5js.OutputPort.prototype.write = function(str) {};
