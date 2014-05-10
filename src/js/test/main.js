goog.provide('r5js.test.evalSandbox');
goog.provide('r5js.test.main');
goog.setTestOnly('r5js.test.main');
goog.setTestOnly('r5js.test.evalSandbox');


goog.require('goog.log');
goog.require('r5js.js.Environment');
goog.require('r5js.Reader');
goog.require('r5js.InputPort');
goog.require('r5js.ParserImpl');
goog.require('r5js.Scanner');
goog.require('r5js.boot');
goog.require('r5js.test.JsInterop');
goog.require('r5js.test.SchemeTestDriver');
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
  r5js.test.SchemeSources.get(jsEnv.fetchUrl.bind(jsEnv)).
      then(function(sources) {
        var publicApi = r5js.test.getEvaluator_(sources);
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
    new r5js.ParserImpl(datumRoot).parse();
  }
};


/** @param {string} text Text to parse. */
r5js.test.evalSandbox = function(text) {
  r5js.test.SchemeSources.get(goog.labs.net.xhr.get).then(function(sources) {
    var publicApi = r5js.test.getEvaluator_(sources);
    console.log(publicApi.evaluate(text));
  });
};


/** @private {r5js.Evaluator} */
r5js.test.evaluator_ = null;


/**
 * @param {!r5js.test.SchemeSources} sources
 * @return {!r5js.Evaluator}
 * @private
 */
r5js.test.getEvaluator_ = function(sources) {
  if (!r5js.test.evaluator_) {
    r5js.test.evaluator_ = r5js.boot(
        sources.syntax, sources.procedures,
        r5js.InputPort.NULL, r5js.OutputPort.NULL);
  }
  return r5js.test.evaluator_;
};


/**
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.test.SchemeSources} sources
 * @return {!Array.<!tdd.TestSuite>}
 * @private
 */
r5js.test.getTestSuites_ = function(evaluator, sources) {
  return [
    new r5js.test.Scanner(),
    new r5js.test.Parser(),
    new r5js.test.JsInterop(evaluator),
    new r5js.test.SchemeTestDriver(evaluator, sources)
  ];
};


goog.exportSymbol('r5js.test.main', r5js.test.main);
// nodejs hack. See comment in goog.promise.testSuiteAdapter.
goog.exportSymbol('setTimeout', setTimeout);
