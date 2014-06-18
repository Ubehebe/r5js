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


goog.provide('haveJsValue');
goog.provide('r5js.test.matchers.HasJsValue_');
goog.setTestOnly('haveJsValue');


goog.require('goog.array');


/**
 * @param {?} value
 * @return {!tdd.matchers.Matcher}
 */
haveJsValue = function(value) {
  return new r5js.test.matchers.HasJsValue_(value);
};



/**
 * @param {?} expectedValue
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.HasJsValue_ = function(expectedValue) {
  /** @const @private */ this.expectedValue_ = expectedValue;
};


/** @override */
r5js.test.matchers.HasJsValue_.prototype.matches = function(actualValue) {
  return r5js.test.matchers.HasJsValue_.equals(
      this.expectedValue_,
      r5js.test.matchers.HasJsValue_.prepare(
          /** @type {!r5js.JsonValue} */ (actualValue)));
};


/** @override */
r5js.test.matchers.HasJsValue_.prototype.getSuccessMessage = function(
    actualValue) {
  return 'ok';
};


/** @override */
r5js.test.matchers.HasJsValue_.prototype.getFailureMessage = function(
    actualValue) {
  return 'want ' +
      this.expectedValue_ +
      ' got ' +
      (/** @type {!r5js.JsonValue} */ (actualValue)).value;
};


/**
 * @param {!r5js.JsonValue} jsonValue
 * @return {?}
 */
r5js.test.matchers.HasJsValue_.prepare = function(jsonValue) {
  var retval = jsonValue.value;
  return retval instanceof Array ?
      retval.map(r5js.test.matchers.HasJsValue_.prepare) :
      retval;
};


/**
 * @param {?} x
 * @param {?} y
 * @return {boolean}
 */
r5js.test.matchers.HasJsValue_.equals = function(x, y) {
  var xIsArray = x instanceof Array;
  var yIsArray = y instanceof Array;
  if (xIsArray && yIsArray) {
    return x.length === y.length &&
        goog.array.zip(x, y).every(function(pair) {
          return r5js.test.matchers.HasJsValue_.equals(pair[0], pair[1]);
        });
  } else if (!(xIsArray || yIsArray)) {
    return x === y;
  } else {
    return false;
  }
};
