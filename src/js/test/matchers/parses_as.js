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

goog.provide('parseAs');
goog.setTestOnly('parseAs');
goog.provide('r5js.test.matchers.ParsesAs');
goog.setTestOnly('r5js.test.matchers.ParsesAs');


goog.require('r5js.Datum');
goog.require('r5js.ParserImpl');
goog.require('r5js.Reader');
goog.require('r5js.Scanner');
goog.require('r5js.parse.bnf');


/**
 * @param {!r5js.parse.Nonterminal} expectedType
 * @return {!tdd.matchers.Matcher}
 */
parseAs = function(expectedType) {
  return new r5js.test.matchers.ParsesAs(expectedType);
};



/**
 * @param {!r5js.parse.Nonterminal} expectedType
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 */
r5js.test.matchers.ParsesAs = function(expectedType) {
  /** @const @private {!r5js.parse.Nonterminal} */
  this.expectedType_ = expectedType;

  /** @private {!r5js.parse.Nonterminal|null} */
  this.actualType_ = null;
};


/** @override */
r5js.test.matchers.ParsesAs.prototype.matches = function(value) {
  var datumRoot;
  try {
    datumRoot = new r5js.Reader(
        new r5js.Scanner(/** @type {string} */ (value))).read();
  } catch (e) {
    return false;
  }
  var actualResult = (datumRoot instanceof r5js.Datum) &&
      new r5js.ParserImpl(datumRoot).parse(this.expectedType_);
  if (actualResult && actualResult.peekParse) {
    this.actualType_ = /** @type {!r5js.parse.Nonterminal} */ (
        actualResult.peekParse());
  }
  return this.actualType_ === this.expectedType_;
};


/** @override */
r5js.test.matchers.ParsesAs.prototype.getSuccessMessage = function(value) {
  return value + ' correctly parses as ' + this.expectedType_;
};


/** @override */
r5js.test.matchers.ParsesAs.prototype.getFailureMessage = function(value) {
  return 'expected ' +
      value +
      ' to parse as ' +
      this.expectedType_ +
      ', got ' +
      this.actualType_;
};
