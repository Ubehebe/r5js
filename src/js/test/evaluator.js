goog.provide('r5js.test.Evaluator');
goog.setTestOnly('r5js.test.Evaluator');


goog.require('expect');
goog.require('haveJsValue');
goog.require('haveStringValue');
goog.require('r5js.test.matchers.setSharedEvaluator');
goog.require('goog.functions');
goog.require('tdd.TestType');



/**
 * @param {!r5js.Evaluator} evaluator
 * @implements {tdd.TestSuite}
 * @struct
 * @constructor
 */
r5js.test.Evaluator = function(evaluator) {
  r5js.test.matchers.setSharedEvaluator(evaluator);
};


/** @override */
r5js.test.Evaluator.prototype.getType = goog.functions.constant(
    tdd.TestType.UNIT);


/** @override */
r5js.test.Evaluator.prototype.toString = goog.functions.constant(
    'r5js.test.Evaluator');


r5js.test.Evaluator.prototype['testSchemeToJsEvaluation'] = function() {
  expect('(+ 1 1)').to(haveJsValue(2));
  expect('(+ 1 1)').to(haveStringValue('2'));
  expect('(procedure? procedure?)').to(haveJsValue(true));
  //  expect('(procedure? procedure?)').to(haveStringValue('#t'));
  //  expect(this.toJs_.evaluate('(string-append "hello " "world")')).
  //      toBe('hello world');
};
