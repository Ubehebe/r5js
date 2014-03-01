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



/** @interface */
r5js.Port = function() {};


r5js.Port.prototype.close = function() {};


/**
 * @return {boolean} True iff the port has a character ready.
 */
r5js.Port.prototype.isCharReady = function() {};


/**
 * @return {?} TODO bl
 */
r5js.Port.prototype.peekChar = function() {};


/**
 * @return {?} TODO bl
 */
r5js.Port.prototype.readChar = function() {};


/** @override */
r5js.Port.prototype.toString = function() {};


/**
 * @param {string} str String to write.
 */
r5js.Port.prototype.write = function(str) {};


/**
 * @param {string} c Character to write.
 */
r5js.Port.prototype.writeChar = function(c) {};
