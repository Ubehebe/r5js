goog.provide('r5js.test.evalSandbox');
goog.provide('r5js.test.main');
goog.provide('r5js.test.main1');
goog.provide('r5js.test.parseSandbox');
goog.provide('r5js.test.readSandbox');
goog.setTestOnly('r5js.test.main');
goog.setTestOnly('r5js.test.main1');
goog.setTestOnly('r5js.test.evalSandbox');
goog.setTestOnly('r5js.test.parseSandbox');
goog.setTestOnly('r5js.test.readSandbox');


goog.require('goog.log');
goog.require('r5js.EvalAdapter');
goog.require('r5js.InMemoryInputPort');
goog.require('r5js.InMemoryOutputPort');
goog.require('r5js.ParserImpl');
goog.require('r5js.Reader');
goog.require('r5js.Scanner');
goog.require('r5js.boot');
goog.require('r5js.js.Environment');
goog.require('r5js.test.JsInterop');
goog.require('r5js.test.Parser');
goog.require('r5js.test.Scanner');
goog.require('r5js.test.SchemeSources');
goog.require('r5js.test.SchemeTestDriver');
goog.require('tdd.Formatter');
goog.require('tdd.Runner');
goog.require('tdd.RunnerConfig');


/**
 * Main entry point for the test suite.
 * @param {!Array.<string>=} opt_argv Optional command-line arguments.
 * @param {!Object.<string, string>=} opt_env Optional command-line environment.
 */
r5js.test.main = function(opt_argv, opt_env) {
  var testConfig = goog.isDef(opt_argv) && goog.isDef(opt_env) ?
      tdd.RunnerConfig.fromFlags(opt_argv, opt_env) :
      r5js.test.main.defaultConfig_();
  r5js.test.main1(testConfig);
};


/**
 * Alternative main entry point where callers can pass in a config object
 * directly, rather than having it constructed from command-line params
 * and environment variables. This is useful for starting the tests from
 * inside a web worker, for example.
 * @param {!tdd.RunnerConfig} testConfig
 */
r5js.test.main1 = function(testConfig) {
  var logger = goog.log.getLogger('r5js.test.main');
  var runner = new tdd.Runner(testConfig, logger);
  var jsEnv = r5js.js.Environment.get();
  r5js.test.SchemeSources.get(jsEnv.fetchUrl.bind(jsEnv)).
      then(function(sources) {
        var evaluator = r5js.test.getEvaluator_(sources);
        r5js.test.getTestSuites_(evaluator, sources).
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


/**
 * @return {!tdd.RunnerConfig}
 * @private
 */
r5js.test.main.defaultConfig_ = function() {
  var formatter = new tdd.Formatter();
  return new tdd.RunnerConfig().
      setTestTypesToRun([tdd.TestType.UNIT, tdd.TestType.INTEGRATION
      ]).addFailureHandler(function(logRecord) {
        console.log(formatter.formatRecord(logRecord));
      }).addSuccessHandler(function(logRecord) {
        console.log(formatter.formatRecord(logRecord));
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
    console.log(r5js.EvalAdapter.toDisplayString(publicApi.evaluate(text)));
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
    var buffer = [];
    var stdin = new r5js.InMemoryInputPort(buffer);
    var stdout = new r5js.InMemoryOutputPort(buffer);
    r5js.test.evaluator_ = r5js.boot(
        sources.syntax, sources.procedures, stdin, stdout);
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
