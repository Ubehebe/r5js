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

goog.provide('r5js.IReader');



/** @interface */
r5js.IReader = function() {};


/**
 * @return {!r5js.Datum} The root of the datum tree.
 * If reading the tokens into datums was unsuccessful, a {@link r5js.ReadError}
 * is thrown.
 * @throws {r5js.ReadError}
 */
r5js.IReader.prototype.read = function() {};
