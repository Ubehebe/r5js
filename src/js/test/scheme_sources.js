goog.provide('r5js.test.SchemeSources');
goog.setTestOnly('r5js.test.SchemeSources');


goog.require('goog.Promise');



/**
 * @param {string} syntax
 * @param {string} procedures
 * @param {string} testFramework
 * @param {string} negativeTestFramework
 * @param {string} r5RSTests
 * @param {string} negativeTests
 * @param {string} otherTests
 * @struct
 * @constructor
 */
r5js.test.SchemeSources = function(
    syntax,
    procedures,
    testFramework,
    negativeTestFramework,
    r5RSTests,
    negativeTests,
    otherTests) {
  /** @const */ this.syntax = syntax;
  /** @const */ this.procedures = procedures;
  /** @const */ this.testFramework = testFramework;
  /** @const */ this.negativeTestFramework = negativeTestFramework;
  /** @const */ this.r5RSTests = r5RSTests;
  /** @const */ this.negativeTests = negativeTests;
  /** @const */ this.otherTests = otherTests;
};


/** @private {r5js.test.SchemeSources} */
r5js.test.SchemeSources.sources_ = null;


/** @return {!goog.Promise.<!r5js.test.SchemeSources>} */
r5js.test.SchemeSources.get = function() {
  if (r5js.test.SchemeSources.sources_) {
    return goog.Promise.resolve(r5js.test.SchemeSources.sources_);
  } else {
    return goog.Promise.all([
      r5js.test.SchemeSources.urls_.SYNTAX,
      r5js.test.SchemeSources.urls_.PROCEDURES,
      r5js.test.SchemeSources.urls_.TEST_FRAMEWORK,
      r5js.test.SchemeSources.urls_.NEGATIVE_TEST_FRAMEWORK,
      r5js.test.SchemeSources.urls_.R5RS_TESTS,
      r5js.test.SchemeSources.urls_.NEGATIVE_TESTS,
      r5js.test.SchemeSources.urls_.OTHER_TESTS
    ].map(function(url) {
      return goog.labs.net.xhr.get(url);
    })).then(function(sources) {
      if (!r5js.test.SchemeSources.sources_) {
        r5js.test.SchemeSources.sources_ = new r5js.test.SchemeSources(
            sources[0],
            sources[1],
            sources[2],
            sources[3],
            sources[4],
            sources[5],
            sources[6]);
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
  SYNTAX: '/src/scm/r5rs-syntax.scm',
  PROCEDURES: '/src/scm/r5rs-procedures.scm',
  TEST_FRAMEWORK: '/test/framework/unit-test.scm',
  NEGATIVE_TEST_FRAMEWORK: '/test/framework/unit-test-negative.scm',
  R5RS_TESTS: '/test/r5rs-tests.scm',
  NEGATIVE_TESTS: '/test/negative-tests.scm',
  OTHER_TESTS: '/test/other-tests.scm'
};

