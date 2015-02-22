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


goog.provide('evalTo');
goog.setTestOnly('evalTo');


goog.require('tdd.matchers.Matcher');


/**
 * @param {string} value
 * @return {!tdd.matchers.Matcher<string>}
 */
evalTo = function(value) {
  return new EvaluatesTo_(value);
};



/**
 * @param {string} expectedValue
 * @implements {tdd.matchers.Matcher<string>}
 * @struct
 * @constructor
 * @private
 */
var EvaluatesTo_ = function(expectedValue) {
  /** @const @private */ this.expectedValue_ = expectedValue;
};


/** @override */
EvaluatesTo_.prototype.matches = function(actualValue) {
  return this.expectedValue_ === actualValue;
};


/** @override */
EvaluatesTo_.prototype.getFailureMessage = function(actualValue) {
  return 'want ' +
      this.expectedValue_ +
      ' got ' +
      actualValue;
};
