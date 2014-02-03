goog.provide('r5js.test.main');


goog.require('goog.debug.Logger');
goog.require('r5js.test.Parser');
goog.require('r5js.test.Scanner');
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
    r5js.test.getTestSuites_().forEach(function(testSuite) {
        runner.add(testSuite);
    });
    runner.run().then(function(result) {
        window.console.log(result.toString());
    });
};

/**
 * @return {!Array.<!tdd.TestSuite>}
 * @private
 */
r5js.test.getTestSuites_ = function() {
  return [
    new r5js.test.Scanner(),
      new r5js.test.Parser()
  ];
};

goog.exportSymbol('r5js.test.main', r5js.test.main);
