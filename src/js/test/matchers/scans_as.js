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

goog.provide('scanAs');
goog.setTestOnly('scanAs');


goog.require('r5js.Scanner');


/**
 * @param {function(new: r5js.Datum, ?)} expectedType
 * @return {!tdd.matchers.Matcher<string>}
 */
scanAs = function(expectedType) {
  return new ScansAs_(expectedType);
};



/**
 * @param {function(new: r5js.Datum, ?)} expectedType
 * @implements {tdd.matchers.Matcher<string>}
 * @struct
 * @constructor
 * @private
 */
var ScansAs_ = function(expectedType) {
  /** @const @private */ this.expectedType_ = expectedType;
};


/** @override */
ScansAs_.prototype.matches = function(value) {
  try {
    var scanner = new r5js.Scanner(value);
    var token = scanner.nextToken();
    // There should be exactly one token in the input.
    // (For example, 1+2 should fail to scan as one number token,
    // even though the whole input scans.)
    if (!token || scanner.nextToken()) {
      return false;
    }
    return token instanceof this.expectedType_;
  } catch (e) {
    return false; // some tests purposely cause scan errors
  }
};


/** @override */
ScansAs_.prototype.getFailureMessage = function(value) {
  return 'expected ' + value + ' to scan as ' + this.expectedType_;
};
