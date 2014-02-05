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
    goog.labs.net.xhr.get(r5js.test.SYNTAX_URL_).then(function(syntaxLib) {
        goog.labs.net.xhr.get(r5js.test.PROCEDURES_URL_).then(function(procedureLib) {
            var publicApi = r5js.test.setupApi_(syntaxLib, procedureLib);
                r5js.test.getTestSuites_(publicApi).forEach(function(testSuite) {
                    runner.add(testSuite);
                });
            runner.run().then(function(result) {
               window.console.log(result.toString());
            });
        });
    });
};


/**
 * @param {string} syntaxLib
 * @param {string} procedureLib
 * @return {!r5js.PublicApi}
 * @private
 */
r5js.test.setupApi_ = function(syntaxLib, procedureLib) {
    var pipeline = new r5js.LazyBoot(
        new r5js.Pipeline(),
        function() {
            r5js.boot(syntaxLib, procedureLib, r5js.util.Logger.getLogger('r5js'));
            pipeline.setRootEnv(/** @type {!r5js.RootEnvironment} */ (
                r5js.globals.r5RSEnv));
        });
    return new r5js.PublicApi(pipeline);
};

/**
 * @param {!r5js.PublicApi} publicApi
 * @return {!Array.<!tdd.TestSuite>}
 * @private
 */
r5js.test.getTestSuites_ = function(publicApi) {
  return [
    new r5js.test.Scanner(),
      new r5js.test.Parser(),
      new r5js.test.Interpreter(publicApi)
  ];
};


/** @const @private {string} */
r5js.test.SYNTAX_URL_ = '/src/scm/r5rs-syntax.scm';

/** @const @private {string} */
r5js.test.PROCEDURES_URL_ = '/src/scm/r5rs-procedures.scm';

goog.exportSymbol('r5js.test.main', r5js.test.main);
