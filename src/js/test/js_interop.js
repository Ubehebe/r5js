goog.provide('r5js.test.JsInterop');
goog.setTestOnly('r5js.test.JsInterop');


goog.require('expect');
goog.require('haveJsValue');
goog.require('haveJsOutput');
goog.require('haveStringOutput');
goog.require('haveStringValue');
goog.require('r5js.test.matchers.setSharedEvaluator');
goog.require('goog.functions');
goog.require('tdd.TestType');



/**
 * Tests exercising Scheme->JavaScript interoperability.
 * @param {!r5js.Evaluator} evaluator
 * @implements {tdd.TestSuite}
 * @struct
 * @constructor
 */
r5js.test.JsInterop = function(evaluator) {
  r5js.test.matchers.setSharedEvaluator(evaluator);
};


/** @override */
r5js.test.JsInterop.prototype.getType = goog.functions.constant(
    tdd.TestType.UNIT);


/** @override */
r5js.test.JsInterop.prototype.toString = goog.functions.constant(
    'r5js.test.JsInterop');


r5js.test.JsInterop.prototype['testReturnPrimitivesToJs'] = function() {
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
  expect('#\\space').to(haveJsValue(' '));
  expect('#\\space').to(haveStringValue('#\\space'));
  expect('#\\newline').to(haveJsValue('\n'));
  expect('#\\newline').to(haveStringValue('#\\newline'));
};


r5js.test.JsInterop.prototype['testDisplayPrimitivesToJs'] = function() {
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
  expect('(display #\\space)').to(haveJsOutput(' '));
  expect('(display #\\space)').to(haveStringOutput(' '));
  expect('(display #\\newline)').to(haveJsOutput('\n'));
  expect('(display #\\newline)').to(haveStringOutput('\n'));
};


r5js.test.JsInterop.prototype['testWritePrimitivesToJs'] = function() {
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
  expect('(write #\\space)').to(haveJsOutput(' '));
  expect('(write #\\space)').to(haveStringOutput('#\\space'));
  expect('(write #\\newline)').to(haveJsOutput('\n'));
  expect('(write #\\newline)').to(haveStringOutput('#\\newline'));
};


r5js.test.JsInterop.prototype['testSanityChecks'] = function() {
  expect('(+ 1 1)').to(haveJsValue(2));
  expect('(+ 1 1)').to(haveStringValue('2'));
  expect('(procedure? procedure?)').to(haveJsValue(true));
  expect('(procedure? procedure?)').to(haveStringValue('#t'));
  expect('(string-append "hello " "world")').to(
      haveJsValue('hello world'));
  expect('(string-append "hello " "world")').to(
      haveStringValue('"hello world"'));
};


r5js.test.JsInterop.prototype['testReturnRecursiveTypesToJs'] = function() {
  expect('#()').to(haveJsValue([]));
  expect('#()').to(haveStringValue('#()'));
  expect("'()").to(haveJsValue([]));
  expect("'()").to(haveStringValue('()'));
  expect("(list '() '() '() '(42))").to(haveJsValue([[], [], [], [42]]));
  expect("(list '() '() '() '(42))").to(haveStringValue('(() () () (42))'));
  expect('(list 1 2 3)').to(haveJsValue([1, 2, 3]));
  expect('(list 1 2 3)').to(haveStringValue('(1 2 3)'));
  expect("(cons 'a (cons 'b (cons 'c '())))").to(haveJsValue(['a', 'b', 'c']));
  expect("(cons 'a (cons 'b (cons 'c '())))").to(haveStringValue('(a b c)'));
  expect("(cons 'a 'b)").not().to(haveJsValue(['a', 'b']));
  expect("(cons 'a 'b)").to(haveStringValue('(a . b)'));
};


r5js.test.JsInterop.prototype['testDisplayRecursiveTypesToJs'] = function() {
  expect('(display #())').to(haveJsOutput([]));
  expect('(display #())').to(haveStringOutput('#()'));
  expect("(display '())").to(haveJsOutput([]));
  expect("(display '())").to(haveStringOutput('()'));
  expect("(display (list '() '() '() '(42)))").
      to(haveJsOutput([[], [], [], [42]]));
  expect("(display (list '() '() '() '(42)))").
      to(haveStringOutput('(() () () (42))'));
  expect('(display (list 1 2 3))').to(haveJsOutput([1, 2, 3]));
  expect('(display (list 1 2 3))').to(haveStringOutput('(1 2 3)'));
  expect("(display (cons 'a (cons 'b (cons 'c '()))))").
      to(haveJsOutput(['a', 'b', 'c']));
  expect("(display (cons 'a (cons 'b (cons 'c '()))))").
      to(haveStringOutput('(a b c)'));
  expect("(display (cons 'a 'b))").to(haveStringOutput('(a . b)'));
};


r5js.test.JsInterop.prototype['testWriteRecursiveTypesToJs'] = function() {
  expect('(write #())').to(haveJsOutput([]));
  expect('(write #())').to(haveStringOutput('#()'));
  expect("(write '())").to(haveJsOutput([]));
  expect("(write '())").to(haveStringOutput('()'));
  expect("(write (list '() '() '() '(42)))").
      to(haveJsOutput([[], [], [], [42]]));
  expect("(write (list '() '() '() '(42)))").
      to(haveStringOutput('(() () () (42))'));
  expect('(write (list 1 2 3))').to(haveJsOutput([1, 2, 3]));
  expect('(write (list 1 2 3))').to(haveStringOutput('(1 2 3)'));
  expect("(write (cons 'a (cons 'b (cons 'c '()))))").
      to(haveJsOutput(['a', 'b', 'c']));
  expect("(write (cons 'a (cons 'b (cons 'c '()))))").
      to(haveStringOutput('(a b c)'));
  expect("(write (cons 'a 'b))").not().to(haveJsOutput(['a', 'b']));
  expect("(write (cons 'a 'b))").to(haveStringOutput('(a . b)'));
};

