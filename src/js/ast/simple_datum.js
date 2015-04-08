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

goog.provide('r5js.ast.SimpleDatum');


goog.require('r5js.ast.Literal');



/**
 * @param {T} payload
 * @extends {r5js.ast.Literal}
 * @struct
 * @constructor
 * @template T
 */
r5js.ast.SimpleDatum = function(payload) {
  r5js.ast.SimpleDatum.base(this, 'constructor', null /* TODO bl compiler bug? */);

  /** @protected {T} */ this.payload = payload;
};
goog.inherits(r5js.ast.SimpleDatum, r5js.ast.Literal);


/**
 * Booleans, characters, and numbers have value equality semantics.
 * @override
 */
r5js.ast.SimpleDatum.prototype.eqv = function(other) {
  return this.constructor === other.constructor && this.payload ===
      (/** @type {!r5js.ast.SimpleDatum} */ (other)).payload;
};


/** @return {T} */
r5js.ast.SimpleDatum.prototype.getPayload = function() {
  return this.payload;
};


/** @param {T} payload */
r5js.ast.SimpleDatum.prototype.setPayload = function(payload) {
  this.payload = payload;
};


/** @override */
r5js.ast.SimpleDatum.prototype.clone = function(parent) {
  const clone = /** @type {!r5js.ast.SimpleDatum} */ (
      r5js.ast.SimpleDatum.base(this, 'clone', parent));
  clone.setPayload(this.payload);
  return clone;
};


/**
 * TODO bl: this is intended to have the exact semantics of the library
 * procedure equal?, but I'm not sure that it does.
 * @param {!r5js.Datum} other Datum to compare against.
 * @return {boolean}
 */
r5js.ast.SimpleDatum.prototype.isEqual = function(other) {
  return other instanceof r5js.ast.SimpleDatum &&
      this.payload === other.payload;
};


/** @override */
r5js.ast.SimpleDatum.prototype.unwrap = function() {
  return this.payload;
};
