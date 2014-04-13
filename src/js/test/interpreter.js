goog.provide('r5js.test.Interpreter');
goog.setTestOnly('r5js.test.Interpreter');


goog.require('expect');



/**
 * @param {!r5js.PublicApi} publicApi
 * @param {!r5js.test.SchemeSources} sources
 * @implements {tdd.TestSuite}
 * @struct
 * @constructor
 */
r5js.test.Interpreter = function(publicApi, sources) {
  /** @const @private */
  this.publicApi_ = publicApi;

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
  this.publicApi_.Eval(this.sources_.testFramework + this.sources_.r5RSTests,
      goog.bind(console.log, console));
};


r5js.test.Interpreter.prototype['testNegativeTests'] = function() {
  this.publicApi_.Eval(
      this.sources_.negativeTestFramework + this.sources_.negativeTests,
      goog.bind(console.log, console));
};


r5js.test.Interpreter.prototype['testOtherTests'] = function() {
  this.publicApi_.Eval(this.sources_.testFramework + this.sources_.otherTests,
      goog.bind(console.log, console));
};
