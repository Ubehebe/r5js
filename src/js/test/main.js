goog.provide('r5js.test.main');


goog.require('goog.debug.Logger');
goog.require('goog.labs.net.xhr');
goog.require('r5js.LazyBoot');
goog.require('r5js.Pipeline');
goog.require('r5js.PublicApi');
goog.require('r5js.boot');
goog.require('r5js.test.Interpreter');
goog.require('r5js.test.Parser');
goog.require('r5js.test.Scanner');
goog.require('r5js.test.SchemeSources');
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
  r5js.test.SchemeSources.get().then(function(sources) {
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
        var r5RSEnv = r5js.boot(
            sources.syntax,
            sources.procedures,
            r5js.util.Logger.getLogger('r5js'));
        pipeline.setRootEnv(r5RSEnv);
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


goog.exportSymbol('r5js.test.main', r5js.test.main);
