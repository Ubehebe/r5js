goog.provide('r5js.test.Evaluator');
goog.setTestOnly('r5js.test.Evaluator');


goog.require('expect');
goog.require('goog.functions');
goog.require('tdd.TestType');



/**
 * @param {!r5js.Evaluator} evaluator
 * @implements {tdd.TestSuite}
 * @struct
 * @constructor
 */
r5js.test.Evaluator = function(evaluator) {
  /** @const @private */ this.evaluator_ = evaluator;
};


/** @override */
r5js.test.Evaluator.prototype.getType = goog.functions.constant(
    tdd.TestType.UNIT);


/** @override */
r5js.test.Evaluator.prototype.toString = goog.functions.constant(
    'r5js.test.Evaluator');


r5js.test.Evaluator.prototype['testSchemeToJsPrimitives'] = function() {
  expect(this.evaluator_.evaluate('(+ 1 1)')).
      toBe(2);
  expect(this.evaluator_.evaluate('(procedure? procedure?)')).
      toBe(true);
  expect(this.evaluator_.evaluate('(string-append "hello " "world")')).
      toBe('hello world');
};
