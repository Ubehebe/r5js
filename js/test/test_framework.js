goog.module('r5js.test.TestFramework');

const {AsyncEvaluator} = require('/js/eval/impl_collect_es6_sources.es6/node_modules/__main__/js/eval/async_evaluator');
const {CallbackBackedPort} = require('/js/io/io_collect_es6_sources.es6/node_modules/__main__/js/io/callback_backed_port');
const {NULL_INPUT_PORT} = require('/js/io/io_collect_es6_sources.es6/node_modules/__main__/js/io/input_port');
const LogLevel = goog.require('tdd.LogLevel');
const LogRecord = goog.require('tdd.LogRecord');
const Logger = goog.require('goog.log.Logger');
const Promise = goog.require('goog.Promise');
const ResultStruct = goog.require('tdd.ResultStruct');
const {SchemeSources} = require('/js/test/scheme_sources_collect_es6_sources.es6/node_modules/__main__/js/test/scheme_sources');

class TestFramework {
  /** @param {!SchemeSources} sources */
  constructor(sources) {
      /** @const @private */ this.sources_ = sources;
      /** @private */ this.actualResult_ = new ResultStruct_('', 0, 0);
      /** @private {?Logger} */ this.logger_ = null;
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
              'r5js.test.TestFramework',
              result.name_));
          this.actualResult_ = this.actualResult_.merge(result);
      }
  }

  execute(logger) {
      this.logger_ = logger;
      const evaluator = new AsyncEvaluator(
          NULL_INPUT_PORT, new CallbackBackedPort(output => this.onWrite_(output)));
      return evaluator.evaluate(this.sources_.testFramework + this.sources_.testFrameworkTests)
          .then(() => resultIsExpected_(this.actualResult_))
          .then(success => success ? new ResultStruct(1, 0, 0) : new ResultStruct(0, 1, 0))
          .then(Promise.resolve);
  }
}

  /**
   * Must be kept manually in sync with the expected results of
   * test/unit-test-tests.scm.
   * @param {!ResultStruct} result
   * @return {boolean}
   * @private
   */
 function resultIsExpected_(result) {
    return result.getNumRun() === 3
        && result.getNumSucceeded() === 2
        && result.getNumFailed() === 1;
}

/**
 * Parses a Scheme test framework output like this:
 * (foo-tests (3 tests) (1 failed))
 * into a {@link tdd.ResultStruct}, returning null if the parse failed.
 * @param {string} str
 * @return {?ResultStruct_}
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

  /** @return {string} */
  getName() {
      return this.name_;
  }
}

exports = {stringToResultStruct, TestFramework};