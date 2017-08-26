goog.module('r5js.test.SchemeTestDriver');
goog.setTestOnly('r5js.test.SchemeTestDriver');

const CallbackBackedPort = goog.require('r5js.CallbackBackedPort');
const InputPort = goog.require('r5js.InputPort');
const LogLevel = goog.require('tdd.LogLevel');
const LogRecord = goog.require('tdd.LogRecord');
const Logger = goog.require('goog.log.Logger');
const ManualTestSuite = goog.require('tdd.ManualTestSuite');
const ResultStruct = goog.require('tdd.ResultStruct');
const SchemeSources = goog.require('r5js.test.SchemeSources');
const TestType = goog.require('tdd.TestType');
const curPlatform = goog.require('r5js.curPlatform');
const {stringToResultStruct, TestFramework} = goog.require('r5js.test.TestFramework');

/**
 * Driver for running the unit tests written in Scheme.
 * @implements {ManualTestSuite}
 */
class SchemeTestDriver {
    constructor() {
        /** @private */ this.result_ = new ResultStruct(0, 0, 0);
        /** @private {Logger} */ this.logger_ = null;
    }

    /** @override */
    getType() {
        return TestType.UNIT;
    }

    /** @override */
    toString() {
        return 'r5js.test.SchemeTestDriver';
    }

    /** @override */
    estimateSize() {
        return 52; // TODO bl how to do this without evaluating?
    }

    /** @override */
    execute(logger) {
        this.logger_ = logger;
        const platform = curPlatform();
        let result = this.result_;
        const onWrite = this.onWrite_.bind(this);
        const sources = SchemeSources.get();
        const evaluator = platform.newEvaluator(InputPort.NULL, new CallbackBackedPort(onWrite));

        return new TestFramework(sources).execute(logger).then(result_ => {
            result = result.merge(result_);
            return evaluator.evaluate(sources.testFramework + sources.r5RSTests);
        }).then(() => evaluator.evaluate(sources.testFramework + sources.negativeTests)
        ).then(() => evaluator.evaluate(sources.testFramework + sources.otherTests)
        ).then(() => this.result_, undefined /* opt_onRejected */, this);
    }

    /**
     * @param {string} str
     * @private
     */
    onWrite_(str) {
        let result = stringToResultStruct(str);
        if (result) {
            this.logger_.logRecord(new LogRecord(
                result.getNumFailed()
                    ? LogLevel.FAILURE
                    : LogLevel.SUCCESS,
                'r5js.test.SchemeTestDriver',
                result.name_));
            this.result_ = this.result_.merge(result);
        } else if (result = stringToFailureMessage_(str)) {
            this.logger_.logRecord(new LogRecord(
                LogLevel.FAILURE,
                'r5js.test.SchemeTestDriver',
                result));
        }
    }
}
ManualTestSuite.addImplementation(SchemeTestDriver);

/**
 * Parses a Scheme test framework output like this:
 * (fail foo-tests (input (+ 1 1)) (want 3) (got 2))
 * into a string, returning null if the parse failed.
 * @param {string} str
 * @return {?string}
 * @private
 */
function stringToFailureMessage_(str) {
  const match = /\(fail .+ \(input (.*)\) \(want (.*)\) \(got (.*)\)\)/.exec(str);
  if (!match) {
    return null;
  }
  const input = match[1];
  const want = match[2];
  const got = match[3];
  return 'input ' + input + ': want ' + want + ', got ' + got;
}

exports = SchemeTestDriver;
