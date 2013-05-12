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


goog.provide('r5js.tmp.output_modes');

goog.provide('r5js.OutputMode');

/**
 * R5RS 6.6.3:
 * write: "Strings that appear in the written representation are en-
 * closed in doublequotes, and within those strings backslash
 * and doublequote characters are escaped by backslashes.
 * Character objects are written using the #\ notation."
 *
 * display: "Strings that appear in the written representation are not enclosed
 * in doublequotes, and no characters are escaped within
 * those strings. Character objects appear in the representation as if written
 * by write-char instead of by write."
 *
 * @enum {number}
 */
r5js.OutputMode = {
    WRITE: 0,
    DISPLAY: 1
};