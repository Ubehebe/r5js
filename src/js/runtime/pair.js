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
goog.provide('r5js.runtime.Pair');


goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.List');
goog.require('r5js.error');
goog.require('r5js.runtime.NIL');



/**
 * @param {!r5js.runtime.Value} car
 * @param {!r5js.runtime.Value=} opt_cdr
 * @implements {r5js.runtime.ObjectValue}
 * @struct
 * @constructor
 */
r5js.runtime.Pair = function(car, opt_cdr) {
  this.car_ = car;
  this.cdr_ = goog.isDef(opt_cdr) ? opt_cdr : r5js.runtime.NIL;
};


/**
 * @param {!r5js.runtime.Value} other
 * @return {boolean}
 */
r5js.runtime.Pair.prototype.eqv = function(other) {
  return this === other;
};


/**
 * @param {function(!r5js.runtime.Value): !r5js.runtime.Value} f
 * @return {!r5js.runtime.Pair}
 */
r5js.runtime.Pair.prototype.map = function(f) {
  if (this.cdr_ === r5js.runtime.NIL) {
    return new r5js.runtime.Pair(f(this.car_));
  } else if (this.cdr_ instanceof r5js.runtime.Pair) {
    return new r5js.runtime.Pair(
        f(this.car_),
        (/** @type {!r5js.runtime.Pair} */ (this.cdr_)).map(f));
  } else {
    throw new r5js.Error(
        r5js.Error.Type.INTERNAL_INTERPRETER_ERROR, 'not a list!');
  }
};


///**
// * @return {r5js.ast.CompoundDatum}
// * @deprecated TODO bl just a shim
// */
//r5js.runtime.Pair.prototype.toList = function() {
//  if (!this.isList()) {
//    throw new r5js.Error(
//        r5js.Error.Type.INTERNAL_INTERPRETER_ERROR, 'not a list!');
//  }
//  var buffer = new r5js.SiblingBuffer();
//  for (var cur = this; cur !== r5js.runtime.NIL; cur = cur.cdr()) {
//    buffer.appendSibling(cur.car());
//  }
//  return buffer.toList(r5js.ast.List);
//};


/** @return {!r5js.runtime.Value} */
r5js.runtime.Pair.prototype.car = function() {
  return this.car_;
};


/** @return {!r5js.runtime.Value} */
r5js.runtime.Pair.prototype.cdr = function() {
  return this.cdr_;
};


/** @param {!r5js.runtime.Value} car */
r5js.runtime.Pair.prototype.setCar = function(car) {
  this.car_ = car;
};


/** @param {!r5js.runtime.Value} cdr */
r5js.runtime.Pair.prototype.setCdr = function(cdr) {
  this.cdr_ = cdr;
};


/**
 * @param {!Array<!r5js.runtime.Value>} array
 * @return {!r5js.runtime.Value}
 */
r5js.runtime.Pair.fromArray = function(array) {
  if (!array.length) {
    return r5js.runtime.NIL;
  }
  var cars = array.map(function(elem) {
    return new r5js.runtime.Pair(elem);
  });
  for (var i = 0; i < cars.length - 1; ++i) {
    cars[i].setCdr(cars[i + 1]);
  }
  return cars[0];
};


/** @return {boolean} */
r5js.runtime.Pair.prototype.isList = function() {
  return this.cdr_ === r5js.runtime.NIL ||
      (this.cdr_ instanceof r5js.runtime.Pair &&
      (/** @type {!r5js.runtime.Pair} */(this.cdr_)).isList());
};
