goog.provide('r5js.test.SchemeTestDriver');
goog.setTestOnly('r5js.test.SchemeTestDriver');


goog.require('goog.Promise');
goog.require('tdd.ResultStruct');
goog.require('tdd.ManualTestSuite');
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
  return goog.Promise.resolve(new tdd.ResultStruct(1, 0, 0));
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
      this.sources_.negativeTestFramework + this.sources_.negativeTests);
};


/** @private */
r5js.test.SchemeTestDriver.prototype.testOtherTests_ = function() {
  this.evaluator_.evaluate(
      this.sources_.testFramework + this.sources_.otherTests);
};
