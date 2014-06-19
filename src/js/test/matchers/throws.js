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

goog.provide('Throw');
goog.setTestOnly('Throw');


goog.require('r5js.error');


/**
 * @param {!r5js.Error} error
 * @return {!tdd.matchers.Matcher.<!r5js.Error>}
 */
Throw = function(error) {
  return new r5js.test.matchers.Throws_(error);
};



/**
 * @param {!r5js.Error} expectedError
 * @implements {tdd.matchers.Matcher.<!r5js.Error>}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.Throws_ = function(expectedError) {
  /** @const @private */ this.expectedError_ = expectedError;
  /** @private */ this.actualError_ = null;
};


/** @override */
r5js.test.matchers.Throws_.prototype.matches = function(actualError) {
  return r5js.error.equals(
      this.expectedError_, this.actualError_ = actualError);
};


/** @override */
r5js.test.matchers.Throws_.prototype.getFailureMessage = function(input) {
  return input +
      ': want\n' +
      this.expectedError_.toString() +
      '\ngot ' +
      (this.actualError_ ?
          ('\n' + this.actualError_.toString()) :
          'no exception');
};
