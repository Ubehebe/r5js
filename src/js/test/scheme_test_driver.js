goog.provide('r5js.test.SchemeTestDriver');
goog.setTestOnly('r5js.test.SchemeTestDriver');


goog.require('goog.Promise');
goog.require('tdd.ResultStruct');
goog.require('tdd.ManualTestSuite');
goog.require('tdd.ResultStruct');
goog.require('expect');
goog.require('r5js.CallbackBackedPort');
goog.require('r5js.EvalAdapter');



/**
 * Driver for running the unit tests written in Scheme.
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.test.SchemeSources} sources
 * @extends {tdd.ManualTestSuite}
 * @struct
 * @constructor
 */
r5js.test.SchemeTestDriver = function(evaluator, sources) {
  goog.base(this);
  /** @const @private */ this.evaluator_ = evaluator.withPorts(
      r5js.InputPort.NULL,
      new r5js.CallbackBackedPort(this.onWrite_.bind(this)));
  /** @const @private */ this.sources_ = sources;
};
goog.inherits(r5js.test.SchemeTestDriver, tdd.ManualTestSuite);


/** @override */
r5js.test.SchemeTestDriver.prototype.getType = function() {
  return tdd.TestType.UNIT;
};


/** @override */
r5js.test.SchemeTestDriver.prototype.toString = function() {
  return 'r5js.test.SchemeTestDriver';
};


/** @override */
r5js.test.SchemeTestDriver.prototype.execute = function(logger) {
  this.testR5RSTests_();
  this.testNegativeTests_();
  this.testOtherTests_();
  return new r5js.test.SchemeTestDriver.TestFrameworkTest_(
      this.evaluator_, this.sources_).execute(logger);
};


/**
 * @param {!r5js.runtime.Value} value
 * @private
 */
r5js.test.SchemeTestDriver.prototype.onWrite_ = function(value) {
  console.log(r5js.EvalAdapter.toWriteString(value));
};


/** @private */
r5js.test.SchemeTestDriver.prototype.testR5RSTests_ = function() {
  this.evaluator_.evaluate(
      this.sources_.testFramework + this.sources_.r5RSTests);
};


/** @private */
r5js.test.SchemeTestDriver.prototype.testNegativeTests_ = function() {
  this.evaluator_.evaluate(
      this.sources_.testFramework + this.sources_.negativeTests);
};


/** @private */
r5js.test.SchemeTestDriver.prototype.testOtherTests_ = function() {
  this.evaluator_.evaluate(
      this.sources_.testFramework + this.sources_.otherTests);
};


/**
 * Parses a Scheme test framework output like this:
 * (foo-tests (3 tests) (1 errors))
 * into a {@link tdd.ResultStruct}, returning null if the parse failed.
 * Uses {@link r5js.EvalAdapter#toJsValue} to avoid messing with the AST.
 * The JavaScript serialization of the above output is this:
 * ["foo-tests", [3, "tests"], [1, "errors"]]
 * @param {!r5js.runtime.Value} output
 * @return {tdd.ResultStruct}
 * @private
 */
r5js.test.SchemeTestDriver.resultStructForOutput_ = function(output) {
  var jsValue = r5js.EvalAdapter.toJsValue(output);
  if (jsValue.length === 3 &&
      goog.isString(jsValue[0]) &&
      jsValue[1].length === 2 &&
      goog.isNumber(jsValue[1][0]) &&
      jsValue[1][1] === 'tests' &&
      jsValue[2].length === 2 &&
      goog.isNumber(jsValue[2][0]) &&
      jsValue[2][1] === 'failed') {
    var numTests = jsValue[1][0];
    var numFailed = jsValue[2][0];
    var numSucceeded = numTests - numFailed;
    return new tdd.ResultStruct(
        numSucceeded, numFailed, 0 /* TODO bl support */);
  } else {
    return null;
  }
};



/**
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.test.SchemeSources} sources
 * @extends {tdd.ManualTestSuite}
 * @struct
 * @constructor
 * @private
 */
r5js.test.SchemeTestDriver.TestFrameworkTest_ = function(evaluator, sources) {
  goog.base(this);
  /** @const @private */ this.evaluator_ = evaluator.withPorts(
      r5js.InputPort.NULL,
      new r5js.CallbackBackedPort(this.onWrite_.bind(this)));

  /** @const @private */ this.sources_ = sources;
  /** @private */ this.actualResult_ = new tdd.ResultStruct(0, 0, 0);
};
goog.inherits(
    r5js.test.SchemeTestDriver.TestFrameworkTest_, tdd.ManualTestSuite);


/**
 * Must be kept manually in sync with the expected results of
 * test/unit-test-tests.scm.
 * @param {!tdd.ResultStruct} result
 * @return {boolean}
 * @private
 */
r5js.test.SchemeTestDriver.TestFrameworkTest_.resultIsExpected_ =
    function(result) {
  return result.getNumRun() === 3 &&
      result.getNumSucceeded() === 2 &&
      result.getNumFailed() === 1;
};


/**
 * @param {!r5js.runtime.Value} value
 * @private
 */
r5js.test.SchemeTestDriver.TestFrameworkTest_.prototype.onWrite_ = function(
    value) {
  var result = r5js.test.SchemeTestDriver.resultStructForOutput_(value);
  if (result) {
    this.actualResult_ = this.actualResult_.merge(result);
  }
};


/** @override */
r5js.test.SchemeTestDriver.TestFrameworkTest_.prototype.getType =
    goog.functions.constant(tdd.TestType.UNIT);


/** @override */
r5js.test.SchemeTestDriver.TestFrameworkTest_.prototype.toString =
    goog.functions.constant('r5js.test.SchemeTestDriver.TestFrameworkTest_');


/** @override */
r5js.test.SchemeTestDriver.TestFrameworkTest_.prototype.execute =
    function(logger) {
  this.evaluator_.evaluate(
      this.sources_.testFramework + this.sources_.testFrameworkTests);
  var success = r5js.test.SchemeTestDriver.TestFrameworkTest_.resultIsExpected_(
      this.actualResult_);
  return goog.Promise.resolve(
      success ?
      new tdd.ResultStruct(1, 0, 0) :
      new tdd.ResultStruct(0, 1, 0));
};
