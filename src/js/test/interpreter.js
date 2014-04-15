goog.provide('r5js.test.Interpreter');
goog.setTestOnly('r5js.test.Interpreter');


goog.require('expect');



/**
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.test.SchemeSources} sources
 * @implements {tdd.TestSuite}
 * @struct
 * @constructor
 */
r5js.test.Interpreter = function(evaluator, sources) {
  /** @const @private */
  this.evaluator_ = evaluator;

  /** @const @private */
  this.sources_ = sources;
};


/** @override */
r5js.test.Interpreter.prototype.getType = function() {
  return tdd.TestType.UNIT;
};


/** @override */
r5js.test.Interpreter.prototype.toString = function() {
  return 'r5js.test.Interpreter';
};


r5js.test.Interpreter.prototype['testR5RSTests'] = function() {
  this.evaluator_.evaluate(
      this.sources_.testFramework + this.sources_.r5RSTests);
};


r5js.test.Interpreter.prototype['testNegativeTests'] = function() {
  this.evaluator_.evaluate(
      this.sources_.negativeTestFramework + this.sources_.negativeTests);
};


r5js.test.Interpreter.prototype['testOtherTests'] = function() {
  this.evaluator_.evaluate(
      this.sources_.testFramework + this.sources_.otherTests);
};
