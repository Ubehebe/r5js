goog.provide('r5js.test.Evaluator');
goog.setTestOnly('r5js.test.Evaluator');


goog.require('expect');
goog.require('haveJsValue');
goog.require('haveJsOutput');
goog.require('haveStringOutput');
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


r5js.test.Evaluator.prototype['testReturnPrimitivesToJs'] = function() {
  expect('42').to(haveJsValue(42));
  expect('42').to(haveStringValue('42'));
  expect('#t').to(haveJsValue(true));
  expect('#t').to(haveStringValue('#t'));
  expect('#f').to(haveJsValue(false));
  expect('#f').to(haveStringValue('#f'));
  expect('"hello, world"').to(haveJsValue('hello, world'));
  expect('"hello, world"').to(haveStringValue('"hello, world"'));
  expect("'hello").to(haveJsValue('hello'));
  expect("'hello").to(haveStringValue('hello'));
  expect('(quote hello)').to(haveJsValue('hello'));
  expect('(quote hello)').to(haveStringValue('hello'));
  expect('#\\a').to(haveJsValue('a'));
  expect('#\\a').to(haveStringValue('#\\a'));
};


r5js.test.Evaluator.prototype['testDisplayPrimitivesToJs'] = function() {
  expect('(display 42)').to(haveJsOutput(42));
  expect('(display 42)').to(haveJsOutput(42));
  expect('(display 42)').to(haveStringOutput('42'));
  expect('(display #t)').to(haveJsOutput(true));
  expect('(display #t)').to(haveStringOutput('#t'));
  expect('(display #f)').to(haveJsOutput(false));
  expect('(display #f)').to(haveStringOutput('#f'));
  expect('(display "hello, world")').to(haveJsOutput('hello, world'));
  expect('(display "hello, world")').to(haveStringOutput('hello, world'));
  expect("(display 'hello)").to(haveJsOutput('hello'));
  expect("(display 'hello)").to(haveStringOutput('hello'));
  expect('(display (quote hello))').to(haveJsOutput('hello'));
  expect('(display (quote hello))').to(haveStringOutput('hello'));
  expect('(display #\\a)').to(haveJsOutput('a'));
  expect('(display #\\a)').to(haveStringOutput('a'));
};


r5js.test.Evaluator.prototype['testWritePrimitivesToJs'] = function() {
  expect('(write 42)').to(haveJsOutput(42));
  expect('(write 42)').to(haveJsOutput(42));
  expect('(write 42)').to(haveStringOutput('42'));
  expect('(write #t)').to(haveJsOutput(true));
  expect('(write #t)').to(haveStringOutput('#t'));
  expect('(write #f)').to(haveJsOutput(false));
  expect('(write #f)').to(haveStringOutput('#f'));
  expect('(write "hello, world")').to(haveJsOutput('hello, world'));
  expect('(write "hello, world")').to(haveStringOutput('"hello, world"'));
  expect("(write 'hello)").to(haveJsOutput('hello'));
  expect("(write 'hello)").to(haveStringOutput('hello'));
  expect('(write (quote hello))').to(haveJsOutput('hello'));
  expect('(write (quote hello))').to(haveStringOutput('hello'));
  expect('(write #\\a)').to(haveJsOutput('a'));
  expect('(write #\\a)').to(haveStringOutput('#\\a'));
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


r5js.test.Evaluator.prototype['testReturnRecursiveTypesToJs'] = function() {
  expect('#()').to(haveJsValue([]));
  expect("'()").to(haveJsValue([]));
  expect("(list '() '() '() '(42))").to(haveJsValue([[], [], [], [42]]));
  expect('(list 1 2 3)').to(haveJsValue([1, 2, 3]));
  expect("(cons 'a (cons 'b (cons 'c '())))").to(haveJsValue(['a', 'b', 'c']));
  expect("(cons 'a 'b)").not().to(haveJsValue(['a', 'b']));
};


r5js.test.Evaluator.prototype['testDisplayRecursiveTypesToJs'] = function() {
  expect('(display #())').to(haveJsOutput([]));
  expect("(display '())").to(haveJsOutput([]));
  expect("(display (list '() '() '() '(42)))").
      to(haveJsOutput([[], [], [], [42]]));
  expect('(display (list 1 2 3))').to(haveJsOutput([1, 2, 3]));
  expect("(display (cons 'a (cons 'b (cons 'c '()))))").
      to(haveJsOutput(['a', 'b', 'c']));
  expect("(display (cons 'a 'b))").not().to(haveJsOutput(['a', 'b']));
};


r5js.test.Evaluator.prototype['testWriteRecursiveTypesToJs'] = function() {
  expect('(write #())').to(haveJsOutput([]));
  expect("(write '())").to(haveJsOutput([]));
  expect("(write (list '() '() '() '(42)))").
      to(haveJsOutput([[], [], [], [42]]));
  expect('(write (list 1 2 3))').to(haveJsOutput([1, 2, 3]));
  expect("(write (cons 'a (cons 'b (cons 'c '()))))").
      to(haveJsOutput(['a', 'b', 'c']));
  expect("(write (cons 'a 'b))").not().to(haveJsOutput(['a', 'b']));
};

