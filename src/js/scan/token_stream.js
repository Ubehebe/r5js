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

goog.provide('r5js.TokenStream');



/** @interface */
r5js.TokenStream = function() {};


/** @return {r5js.Token} The next token, or null if there are no more. */
r5js.TokenStream.prototype.nextToken = function() {};


/** @typedef {number} */
r5js.TokenStream.Checkpoint;


/**
 * Establishes a checkpoint that can be restored by
 * {@link r5js.TokenStream#restore}.
 * @return {!r5js.TokenStream.Checkpoint}
 */
r5js.TokenStream.prototype.checkpoint = function() {};


/**
 * Restores the state of the token stream represented by checkpoint.
 * @param {!r5js.TokenStream.Checkpoint} checkpoint
 */
r5js.TokenStream.prototype.restore = function(checkpoint) {};
