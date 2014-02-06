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
  /** @const @private {!r5js.PublicApi} */
  this.publicApi_ = publicApi;

  /** @const @private {string} */
  this.testFramework_ = sources.testFramework;

  /** @const @private {string} */
  this.r5RSTests_ = sources.r5RSTests;

  /** @const @private {string} */
  this.otherTests_ = sources.otherTests;

  /** @const @private {!r5js.util.Logger} */
  this.logger_ = r5js.util.Logger.getLogger('r5js.test.Interpreter');
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
  this.publicApi_.Eval(this.testFramework_ + this.r5RSTests_,
      goog.bind(window.console.log, window.console), this.logger_);
};


r5js.test.Interpreter.prototype['testOtherTests'] = function() {
  this.publicApi_.Eval(this.testFramework_ + this.otherTests_,
      goog.bind(window.console.log, window.console), this.logger_);
};
