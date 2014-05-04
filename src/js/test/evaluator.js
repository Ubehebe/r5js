goog.provide('r5js.test.Evaluator');
goog.setTestOnly('r5js.test.Evaluator');


goog.require('expect');
goog.require('haveJsValue');
goog.require('haveJsOutput');
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


r5js.test.Evaluator.prototype['testPrimitives'] = function() {
  expect('42').to(haveJsValue(42));
  expect('42').to(haveStringValue('42'));
  expect('#t').to(haveJsValue(true));
  expect('#t').to(haveStringValue('#t'));
  expect('#f').to(haveJsValue(false));
  expect('#f').to(haveStringValue('#f'));
  expect('"hello, world"').to(haveJsValue('hello, world'));
  expect('"hello, world"').to(haveStringValue('"hello, world"'));
  expect('#\\a').to(haveJsValue('a'));
  expect('#\\a').to(haveStringValue('#\\a'));
};


r5js.test.Evaluator.prototype['testSanityChecks'] = function() {
  expect('(+ 1 1)').to(haveJsValue(2));
  expect('(+ 1 1)').to(haveStringValue('2'));
  expect('(procedure? procedure?)').to(haveJsValue(true));
  expect('(procedure? procedure?)').to(haveStringValue('#t'));
  expect('(string-append "hello " "world")').to(
      haveJsValue('hello world'));
  expect('(string-append "hello " "world")').to(
      haveStringValue('"hello world"'));
};


r5js.test.Evaluator.prototype['testSchemeListToJsArray'] = function() {
  expect('#()').to(haveJsValue([]));
  expect("'()").to(haveJsValue([]));
  expect("(list '() '() '() '(42))").to(haveJsValue([[], [], [], [42]]));
  expect('(list 1 2 3)').to(haveJsValue([1, 2, 3]));
  expect("(cons 'a (cons 'b (cons 'c '())))").to(haveJsValue(['a', 'b', 'c']));
  expect("(cons 'a 'b)").not().to(haveJsValue(['a', 'b']));
};


r5js.test.Evaluator.prototype['testWriteToJs'] = function() {
  expect("(write 'hello)").to(haveJsOutput('hello'));
};
