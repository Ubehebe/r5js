goog.provide('r5js.test.main');


goog.require('goog.debug.Logger');
goog.require('r5js.test.Provider');
goog.require('tdd.Runner');


/**
 * Main entry point for the test suite.
 */
r5js.test.main = function() {
    var logger = goog.debug.Logger.getLogger('sg.test');
    var runner = new tdd.Runner(logger).setVerbose(true);
    var testProvider = new r5js.test.Provider();
    testProvider.getTestSuites().forEach(function(testSuite) {
        runner.add(testSuite);
    });
    runner.run(function(result) {
        window.console.log(result.toString());
    });
};
goog.exportSymbol('r5js.test.main', r5js.test.main);
