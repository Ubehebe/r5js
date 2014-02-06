goog.provide('r5js.test.main');


goog.require('goog.debug.Logger');
goog.require('goog.labs.net.xhr');
goog.require('r5js.boot');
goog.require('r5js.globals');
goog.require('r5js.LazyBoot');
goog.require('r5js.Pipeline');
goog.require('r5js.PublicApi');
goog.require('r5js.test.Interpreter');
goog.require('r5js.test.Parser');
goog.require('r5js.test.Scanner');
goog.require('r5js.util.Logger');
goog.require('tdd.Runner');
goog.require('tdd.RunnerConfig');


/**
 * Main entry point for the test suite.
 * @param {!Array.<string>=} opt_argv Optional command-line arguments.
 */
r5js.test.main = function(opt_argv) {
    var testConfig = goog.isDef(opt_argv) ?
        tdd.RunnerConfig.fromFlags(opt_argv) :
        tdd.RunnerConfig.DEFAULT;
    var logger = goog.debug.Logger.getLogger('r5js.test.main');
    var runner = new tdd.Runner(testConfig, logger);
    r5js.test.getSchemeSources().then(function(sources) {
            var publicApi = r5js.test.setupApi_(sources);
                r5js.test.getTestSuites_(publicApi, sources).forEach(function(testSuite) {
                    runner.add(testSuite);
                });
            runner.run().then(function(result) {
               window.console.log(result.toString());
            });
    });
};


/**
 * @param {!r5js.test.SchemeSources} sources
 * @return {!r5js.PublicApi}
 * @private
 */
r5js.test.setupApi_ = function(sources) {
    var pipeline = new r5js.LazyBoot(
        new r5js.Pipeline(),
        function() {
            r5js.boot(sources.syntax, sources.procedures, r5js.util.Logger.getLogger('r5js'));
            pipeline.setRootEnv(/** @type {!r5js.RootEnvironment} */ (
                r5js.globals.r5RSEnv));
        });
    return new r5js.PublicApi(pipeline);
};

/**
 * @param {!r5js.PublicApi} publicApi
 * @param {!r5js.test.SchemeSources} sources
 * @return {!Array.<!tdd.TestSuite>}
 * @private
 */
r5js.test.getTestSuites_ = function(publicApi, sources) {
  return [
    new r5js.test.Scanner(),
      new r5js.test.Parser(),
      new r5js.test.Interpreter(publicApi, sources)
  ];
};


/**
 * @param {string} syntax
 * @param {string} procedures
 * @param {string} testFramework
 * @param {string} r5RSTests
 * @struct
 * @constructor
 */
r5js.test.SchemeSources = function(syntax, procedures, testFramework, r5RSTests) {
    /** @const {string} */
    this.syntax = syntax;

    /** @const {string} */
    this.procedures = procedures;

    /** @const {string} */
    this.testFramework = testFramework;

    /** @const {string} */
    this.r5RSTests = r5RSTests;
};


/** @return {!goog.labs.Promise.<!r5js.test.SchemeSources>} */
r5js.test.getSchemeSources = function() {
    return goog.labs.Promise.all([
        goog.labs.net.xhr.get(r5js.test.urls_.SYNTAX),
        goog.labs.net.xhr.get(r5js.test.urls_.PROCEDURES),
        goog.labs.net.xhr.get(r5js.test.urls_.TEST_FRAMEWORK),
        goog.labs.net.xhr.get(r5js.test.urls_.R5RS_TESTS)
    ]).then(function(sources) {
            return new r5js.test.SchemeSources(
                sources[0],
                sources[1],
                sources[2],
                sources[3]);
        });
};


/**
 * @enum {string}
 * @private
 */
r5js.test.urls_ = {
SYNTAX: '/src/scm/r5rs-syntax.scm',
    PROCEDURES: '/src/scm/r5rs-procedures.scm',
TEST_FRAMEWORK: '/test/framework/unit-test.scm',
    R5RS_TESTS:  '/test/r5rs-tests.scm'
};




goog.exportSymbol('r5js.test.main', r5js.test.main);
