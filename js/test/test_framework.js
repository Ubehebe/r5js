goog.module('r5js.test.TestFramework');

const CallbackBackedPort = goog.require('r5js.CallbackBackedPort');
const InputPort = goog.require('r5js.InputPort');
const LogLevel = goog.require('tdd.LogLevel');
const LogRecord = goog.require('tdd.LogRecord');
const Logger = goog.require('goog.log.Logger');
const ManualTestSuite = goog.require('tdd.ManualTestSuite');
const Promise = goog.require('goog.Promise');
const ResultStruct = goog.require('tdd.ResultStruct');
const SchemeSources = goog.require('r5js.test.SchemeSources');
const TestType = goog.require('tdd.TestType');
const curPlatform = goog.require('r5js.curPlatform');

/** @implements {ManualTestSuite} */
class TestFramework {
  /** @param {!SchemeSources} sources */
  constructor(sources) {
      /** @const @private */ this.sources_ = sources;
      /** @private */ this.actualResult_ = new ResultStruct_('', 0, 0);
      /** @private {Logger} */ this.logger_ = null;
  }

  /**
   * Must be kept manually in sync with the expected results of
   * test/unit-test-tests.scm.
   * @param {!ResultStruct} result
   * @return {boolean}
   * @private
   */
  static resultIsExpected_(result) {
      return result.getNumRun() === 3
          && result.getNumSucceeded() === 2
          && result.getNumFailed() === 1;
  }

  /**
   * @param {string} str
   * @private
   */
  onWrite_(str) {
      const result = stringToResultStruct(str);
      if (result) {
          this.logger_.logRecord(new LogRecord(
              LogLevel.SUCCESS,
              'r5js.test.SchemeTestDriver',
              result.name_));
          this.actualResult_ = this.actualResult_.merge(result);
      }
  }

  /** @override */
  getType() {
      return TestType.UNIT;
  }


  /** @override */
  estimateSize() {
      return 1;
  }

  /** @override */
  toString() {
      return 'r5js.test.SchemeTestDriver.TestFramework_';
  }

  /** @override */
  execute(logger) {
      this.logger_ = logger;
      const evaluator = curPlatform().newEvaluator(
          InputPort.NULL, new CallbackBackedPort(this.onWrite_.bind(this)));
      return evaluator.evaluate(this.sources_.testFramework + this.sources_.testFrameworkTests)
          .then(() => TestFramework.resultIsExpected_(this.actualResult_),
          undefined /* opt_onRejected */, this)
          .then(success => success
              ? new ResultStruct(1, 0, 0)
              : new ResultStruct(0, 1, 0))
          .then(Promise.resolve);
  }
}
ManualTestSuite.addImplementation(TestFramework);

/**
 * Parses a Scheme test framework output like this:
 * (foo-tests (3 tests) (1 failed))
 * into a {@link tdd.ResultStruct}, returning null if the parse failed.
 * @param {string} str
 * @return {ResultStruct_}
 */
function stringToResultStruct(str) {
  const regex = /\((.+) \((\d+) tests\) \((\d+) failed\)\)/;
  const matches = regex.exec(str);
  if (!matches) {
    return null;
  }
  const name = matches[1];
  const numSucceeded = parseInt(matches[2], 10);
  const numFailed = parseInt(matches[3], 10);
  return new ResultStruct_(name, numSucceeded, numFailed);
}

class ResultStruct_ extends ResultStruct {
  /**
   * @param {string} name
   * @param {number} numSucceeded
   * @param {number} numFailed
   */
  constructor(name, numSucceeded, numFailed) {
      super(numSucceeded, numFailed, 0 /* TODO bl */);
      /** @const @private */ this.name_ = name;
  }
}

exports = {stringToResultStruct, TestFramework};