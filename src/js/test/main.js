/* Copyright 2011-2014 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

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
goog.require('r5js.InMemoryInputPort');
goog.require('r5js.InMemoryOutputPort');
goog.require('r5js.InMemoryPortBuffer');
goog.require('r5js.ParserImpl');
goog.require('r5js.ReaderImpl');
goog.require('r5js.Scanner');
goog.require('r5js.curPlatform');
goog.require('r5js.test.JsInterop');
goog.require('r5js.test.Parser');
goog.require('r5js.test.Scanner');
goog.require('r5js.test.SchemeSources');
goog.require('r5js.test.SchemeTestDriver');
goog.require('r5js.valutil');
goog.require('tdd.Runner');
goog.require('tdd.RunnerConfig');
goog.require('tdd.logTo');


/**
 * Main entry point for the test suite.
 * @param {!Array<string>=} opt_argv Optional command-line arguments.
 * @param {!Object<string, string>=} opt_env Optional command-line environment.
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
  var platform = r5js.curPlatform();

  var buffer = new r5js.InMemoryPortBuffer();
  var stdin = new r5js.InMemoryInputPort(buffer);
  var stdout = new r5js.InMemoryOutputPort(buffer);

  r5js.test.getEvaluator_(stdin, stdout).then(function(evaluator) {
    r5js.test.getTestSuites_(evaluator, stdout).
        forEach(function(testSuite) {
          runner.add(testSuite);
        });
    runner.run().then(function(result) {
      console.log(result.toString());
      platform.exit(
          result.getNumFailed() + result.getNumExceptions() === 0 ?
              0 : 1);
    });
  });
};


/**
 * @return {!tdd.RunnerConfig}
 * @private
 */
r5js.test.main.defaultConfig_ = function() {
  var logWriter = tdd.logTo(goog.global.console);
  return new tdd.RunnerConfig().
      setTestTypesToRun([tdd.TestType.UNIT, tdd.TestType.INTEGRATION])
      .addFailureHandler(logWriter)
      .addSuccessHandler(logWriter);
};


/** @param {string} text Text to read. */
r5js.test.readSandbox = function(text) {
  new r5js.ReaderImpl(new r5js.Scanner(text)).read();
};


/** @param {string} text Text to parse. */
r5js.test.parseSandbox = function(text) {
  var datumRoot = new r5js.ReaderImpl(new r5js.Scanner(text)).read();
  if (datumRoot) {
    new r5js.ParserImpl(datumRoot).parse();
  }
};


/** @param {string} text Text to parse. */
r5js.test.evalSandbox = function(text) {
  r5js.test.getEvaluator_().
      then(function(evaluator) { return evaluator.evaluate(text); }).
      then(function(displayString) { console.log(displayString); });
};


/** @private {goog.Promise<!r5js.Evaluator>} */
r5js.test.evaluator_ = null;


/**
 * @param {!r5js.InputPort=} opt_inputPort
 * @param {!r5js.OutputPort=} opt_outputPort
 * @return {!goog.Promise<!r5js.Evaluator>}
 * @private
 */
r5js.test.getEvaluator_ = function(opt_inputPort, opt_outputPort) {
  return r5js.test.evaluator_ ||
      (r5js.test.evaluator_ = r5js.curPlatform().newEvaluator(
      opt_inputPort, opt_outputPort));
};


/**
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.OutputSavingPort} outputPort
 * @return {!Array<!tdd.TestSuite>}
 * @private
 */
r5js.test.getTestSuites_ = function(evaluator, outputPort) {
  return [
    new r5js.test.Scanner(),
    new r5js.test.Parser(),
    new r5js.test.JsInterop(evaluator, outputPort),
    new r5js.test.SchemeTestDriver()
  ];
};


goog.exportSymbol('r5js.test.main', r5js.test.main);
// nodejs hack. See comment in goog.promise.testSuiteAdapter.
goog.exportSymbol('setTimeout', setTimeout);


if (!goog.global.console) {
  /**
    * nodejs hack. To run the tests in uncompiled mode, we could use
    * Closure's bootstrap/nodejs.js, which sets up goog.global.console.
    * But that file is annotated with @nocompile, so it doesn't work for
    * the compiled tests.
    */
  goog.global.console = console;
}

