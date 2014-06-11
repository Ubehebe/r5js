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


/** @private {r5js.test.SchemeSources} */
r5js.test.SchemeSources.sources_ = null;


/**
 * @param {function(string):!goog.Promise.<string>} urlFetcher
 * @return {!goog.Promise.<!r5js.test.SchemeSources>}
 */
r5js.test.SchemeSources.get = function(urlFetcher) {
  if (r5js.test.SchemeSources.sources_) {
    return goog.Promise.resolve(r5js.test.SchemeSources.sources_);
  } else {
    return goog.Promise.all([
      r5js.test.SchemeSources.urls_.TEST_FRAMEWORK,
      r5js.test.SchemeSources.urls_.TEST_FRAMEWORK_TESTS,
      r5js.test.SchemeSources.urls_.R5RS_TESTS,
      r5js.test.SchemeSources.urls_.NEGATIVE_TESTS,
      r5js.test.SchemeSources.urls_.OTHER_TESTS
    ].map(function(url) {
      return urlFetcher(url);
    })).then(function(sources) {
      if (!r5js.test.SchemeSources.sources_) {
        r5js.test.SchemeSources.sources_ = new r5js.test.SchemeSources(
            sources[0],
            sources[1],
            sources[2],
            sources[3],
            sources[4]);
      }
      return r5js.test.SchemeSources.sources_;
    });
  }
};


/**
 * @enum {string}
 * @private
 */
r5js.test.SchemeSources.urls_ = {
  TEST_FRAMEWORK: '/src/scm/unit-test.scm',
  TEST_FRAMEWORK_TESTS: '/src/scm/unit-test-tests.scm',
  R5RS_TESTS: '/src/scm/r5rs-tests.scm',
  NEGATIVE_TESTS: '/src/scm/negative-tests.scm',
  OTHER_TESTS: '/src/scm/other-tests.scm'
};

