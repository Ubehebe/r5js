goog.provide('r5js.test.evalSandbox');
goog.provide('r5js.test.main');
goog.setTestOnly('r5js.test.main');
goog.setTestOnly('r5js.test.evalSandbox');


goog.require('goog.log');
goog.require('r5js.js.Environment');
goog.require('r5js.LazyBoot');
goog.require('r5js.Pipeline');
goog.require('r5js.PublicApi');
goog.require('r5js.Reader');
goog.require('r5js.Scanner');
goog.require('r5js.boot');
goog.require('r5js.test.Interpreter');
goog.require('r5js.test.Parser');
goog.require('r5js.test.Scanner');
goog.require('r5js.test.SchemeSources');
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
  var logger = goog.log.getLogger('r5js.test.main');
  var runner = new tdd.Runner(testConfig, logger);
  var jsEnv = r5js.js.Environment.get();
  r5js.test.SchemeSources.get(goog.bind(jsEnv.fetchUrl, jsEnv)).
      then(function(sources) {
        var publicApi = r5js.test.getApi_(sources);
        r5js.test.getTestSuites_(publicApi, sources).
            forEach(function(testSuite) {
              runner.add(testSuite);
            });
        runner.run().then(function(result) {
          console.log(result.toString());
          jsEnv.exit(
              result.getNumFailed() + result.getNumExceptions() === 0 ? 0 : 1);
        });
      });
};


/** @param {string} text Text to read. */
r5js.test.readSandbox = function(text) {
  new r5js.Reader(new r5js.Scanner(text)).read();
};


/** @param {string} text Text to parse. */
r5js.test.parseSandbox = function(text) {
  var datumRoot = new r5js.Reader(new r5js.Scanner(text)).read();
  if (datumRoot) {
    new r5js.Parser(datumRoot).parse();
  }
};


/** @param {string} text Text to parse. */
r5js.test.evalSandbox = function(text) {
  r5js.test.SchemeSources.get(goog.labs.net.xhr.get).then(function(sources) {
    var publicApi = r5js.test.getApi_(sources);
    console.log(
        publicApi.Eval(text, goog.nullFunction /* sideEffectHandler */));
  });
};


/** @private {r5js.PublicApi} */
r5js.test.api_ = null;


/**
 * @param {!r5js.test.SchemeSources} sources
 * @return {!r5js.PublicApi}
 * @private
 */
r5js.test.getApi_ = function(sources) {
  if (!r5js.test.api_) {
    var pipeline = new r5js.LazyBoot(
        new r5js.Pipeline(),
        function() {
          var r5RSEnv = r5js.boot(
              sources.syntax,
              sources.procedures,
              goog.log.getLogger('r5js'));
          pipeline.setRootEnv(r5RSEnv);
        });
    r5js.test.api_ = new r5js.PublicApi(pipeline);
  }
  return r5js.test.api_;
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
// nodejs hack. See comment in goog.promise.testSuiteAdapter.
goog.exportSymbol('setTimeout', setTimeout);
