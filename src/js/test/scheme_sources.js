goog.provide('r5js.test.SchemeSources');
goog.setTestOnly('r5js.test.SchemeSources');



/**
 * @param {string} syntax
 * @param {string} procedures
 * @param {string} testFramework
 * @param {string} r5RSTests
 * @param {string} otherTests
 * @struct
 * @constructor
 */
r5js.test.SchemeSources = function(
    syntax, procedures, testFramework, r5RSTests, otherTests) {
  /** @const {string} */
  this.syntax = syntax;
  /** @const {string} */
  this.procedures = procedures;
  /** @const {string} */
  this.testFramework = testFramework;
  /** @const {string} */
  this.r5RSTests = r5RSTests;
  /** @const {string} */
  this.otherTests = otherTests;
};


/** @return {!goog.labs.Promise.<!r5js.test.SchemeSources>} */
r5js.test.SchemeSources.get = function() {
  return goog.labs.Promise.all([
    r5js.test.SchemeSources.urls_.SYNTAX,
    r5js.test.SchemeSources.urls_.PROCEDURES,
    r5js.test.SchemeSources.urls_.TEST_FRAMEWORK,
    r5js.test.SchemeSources.urls_.R5RS_TESTS,
    r5js.test.SchemeSources.urls_.OTHER_TESTS
  ].map(function(url) {
    return goog.labs.net.xhr.get(url);
  })).then(function(sources) {
    return new r5js.test.SchemeSources(
        sources[0],
        sources[1],
        sources[2],
        sources[3],
        sources[4]);
  });
};


/**
 * @enum {string}
 * @private
 */
r5js.test.SchemeSources.urls_ = {
  SYNTAX: '/src/scm/r5rs-syntax.scm',
  PROCEDURES: '/src/scm/r5rs-procedures.scm',
  TEST_FRAMEWORK: '/test/framework/unit-test.scm',
  R5RS_TESTS: '/test/r5rs-tests.scm',
  OTHER_TESTS: '/test/other-tests.scm'
};

