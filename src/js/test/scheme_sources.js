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

goog.provide('r5js.test.SchemeSources');
goog.setTestOnly('r5js.test.SchemeSources');


goog.require('NEGATIVE_TESTS');
goog.require('OTHER_TESTS');
goog.require('R5RS_TESTS');
goog.require('TEST_FRAMEWORK');
goog.require('TEST_FRAMEWORK_TESTS');
goog.require('goog.Promise');



/**
 * @param {string} testFramework
 * @param {string} testFrameworkTests
 * @param {string} r5RSTests
 * @param {string} negativeTests
 * @param {string} otherTests
 * @struct
 * @constructor
 */
r5js.test.SchemeSources = function(
    testFramework,
    testFrameworkTests,
    r5RSTests,
    negativeTests,
    otherTests) {
  /** @const */ this.testFramework = testFramework;
  /** @const */ this.testFrameworkTests = testFrameworkTests;
  /** @const */ this.r5RSTests = r5RSTests;
  /** @const */ this.negativeTests = negativeTests;
  /** @const */ this.otherTests = otherTests;
};


/**
 * @param {function(string):!goog.Promise<string>} urlFetcher
 * @return {!goog.Promise<!r5js.test.SchemeSources>}
 */
r5js.test.SchemeSources.get = function(urlFetcher) {
  return goog.Promise.resolve(
      new r5js.test.SchemeSources(
      TEST_FRAMEWORK,
      TEST_FRAMEWORK_TESTS,
      R5RS_TESTS,
      NEGATIVE_TESTS,
      OTHER_TESTS));
};
