goog.module('r5js.test.main');
goog.setTestOnly('r5js.test.main');

const Evaluator = goog.require('r5js.Evaluator');
const InMemoryInputPort = goog.require('r5js.InMemoryInputPort');
const InMemoryOutputPort = goog.require('r5js.InMemoryOutputPort');
const InMemoryPortBuffer = goog.require('r5js.InMemoryPortBuffer');
const InputPort = goog.require('r5js.InputPort');
const JsInteropTest = goog.require('r5js.test.JsInterop');
const OutputPort = goog.require('r5js.OutputPort');
const OutputSavingPort = goog.require('r5js.OutputSavingPort');
const Runner = goog.require('tdd.Runner');
const RunnerConfig = goog.require('tdd.RunnerConfig');
const SchemeTestDriver = goog.require('r5js.test.SchemeTestDriver');
const TestSuite = goog.require('tdd.TestSuite');
const TestType = goog.require('tdd.TestType');
const curPlatform = goog.require('r5js.curPlatform');
const log = goog.require('goog.log');
const logTo = goog.require('tdd.logTo');

/**
 * Main entry point for the test suite.
 * @param {!Array<string>=} opt_argv Optional command-line arguments.
 * @param {!Object<string, string>=} opt_env Optional command-line environment.
 */
function main(opt_argv, opt_env) {
    const testConfig = goog.isDef(opt_argv) && goog.isDef(opt_env)
        ? RunnerConfig.fromFlags(opt_argv, opt_env)
        : defaultConfig();
    const logger = log.getLogger('r5js.test.main');
    const runner = new Runner(testConfig, logger);
    const platform = curPlatform();
    const buffer = new InMemoryPortBuffer();
    const stdin = new InMemoryInputPort(buffer);
    const stdout = new InMemoryOutputPort(buffer);
    const evaluator = getEvaluator(stdin, stdout);
    getTestSuites(evaluator, stdout).forEach(testSuite => runner.add(testSuite));
    runner.run().then(result => {
        console.log(result.toString());
        platform.exit(result.getNumFailed() + result.getNumExceptions() === 0 ? 0 : 1);
    });
}

/** @return {!RunnerConfig} */
function defaultConfig() {
    const logWriter = logTo(goog.global.console);
    return new RunnerConfig()
        .setTestTypesToRun([TestType.UNIT, TestType.INTEGRATION])
        .addFailureHandler(logWriter)
        .addSuccessHandler(logWriter);
}

/** @type {Evaluator} */ let evaluator = null;

/**
 * @param {!InputPort=} opt_inputPort
 * @param {!OutputPort=} opt_outputPort
 * @return {!Evaluator}
 */
function getEvaluator(opt_inputPort, opt_outputPort) {
  return evaluator
      || (evaluator = curPlatform().newEvaluator(opt_inputPort, opt_outputPort));
}

/**
 * @param {!Evaluator} evaluator
 * @param {!OutputSavingPort} outputPort
 * @return {!Array<!TestSuite>}
 */
function getTestSuites(evaluator, outputPort) {
  return [
    new JsInteropTest(evaluator, outputPort),
    new SchemeTestDriver()
  ];
}

goog.exportSymbol('r5js.test.main', main);
 //nodejs hack. See comment in goog.promise.testSuiteAdapter.
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