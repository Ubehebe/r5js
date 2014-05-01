goog.provide('r5js.test.SchemeTestDriver');
goog.setTestOnly('r5js.test.SchemeTestDriver');


goog.require('expect');



/**
 * Driver for running the unit tests written in Scheme.
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.test.SchemeSources} sources
 * @implements {tdd.TestSuite}
 * @struct
 * @constructor
 */
r5js.test.SchemeTestDriver = function(evaluator, sources) {
  /** @const @private */ this.evaluator_ = evaluator;
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
