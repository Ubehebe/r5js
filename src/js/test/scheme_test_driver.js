goog.provide('r5js.test.SchemeTestDriver');
goog.setTestOnly('r5js.test.SchemeTestDriver');


goog.require('expect');
goog.require('r5js.CallbackBackedPort');



/**
 * Driver for running the unit tests written in Scheme.
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.test.SchemeSources} sources
 * @implements {tdd.TestSuite}
 * @struct
 * @constructor
 */
r5js.test.SchemeTestDriver = function(evaluator, sources) {
  /** @const @private */ this.evaluator_ = evaluator.withPorts(
      r5js.InputPort.NULL,
      new r5js.CallbackBackedPort(this.onWrite_.bind(this)));
  /** @const @private */ this.sources_ = sources;
};


/** @override */
r5js.test.SchemeTestDriver.prototype.getType = function() {
  return tdd.TestType.UNIT;
};


/** @override */
r5js.test.SchemeTestDriver.prototype.toString = function() {
  return 'r5js.test.SchemeTestDriver';
};


/**
 * @param {!r5js.runtime.Value} value
 * @private
 */
r5js.test.SchemeTestDriver.prototype.onWrite_ = function(value) {
  console.log(value);
};


r5js.test.SchemeTestDriver.prototype['testR5RSTests'] = function() {
  this.evaluator_.evaluate(
      this.sources_.testFramework + this.sources_.r5RSTests);
};


r5js.test.SchemeTestDriver.prototype['testNegativeTests'] = function() {
  this.evaluator_.evaluate(
      this.sources_.negativeTestFramework + this.sources_.negativeTests);
};


r5js.test.SchemeTestDriver.prototype['testOtherTests'] = function() {
  this.evaluator_.evaluate(
      this.sources_.testFramework + this.sources_.otherTests);
};
