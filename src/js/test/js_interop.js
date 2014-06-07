/* Copyright 2011-2014 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

goog.provide('r5js.test.JsInterop');
goog.setTestOnly('r5js.test.JsInterop');


goog.require('Throw2');
goog.require('expect');
goog.require('goog.functions');
goog.require('r5js.DatumType');
goog.require('haveJsOutput');
goog.require('haveJsValue');
goog.require('r5js.parse.Terminals');
goog.require('haveStringOutput');
goog.require('haveStringValue');
goog.require('r5js.ArgumentTypeError');
goog.require('r5js.ImmutableError');
goog.require('r5js.IncorrectNumArgs');
goog.require('r5js.ReadError');
goog.require('r5js.TooFewVarargs');
goog.require('r5js.TooManyVarargs');
goog.require('r5js.UnboundVariable');
goog.require('r5js.UnimplementedOptionError');
goog.require('r5js.test.matchers.setSharedEvaluator');
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
  expect("'a").to(haveStringValue('a'));
  expect("''a").to(haveStringValue("'a"));
  expect("'''a").to(haveStringValue("''a"));
  expect("''''a").to(haveStringValue("'''a"));
  expect("'''''a").to(haveStringValue("''''a"));
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


/**
 * R5RS doesn't actually forbid these external representations to be
 * the empty string, but empty strings are not helpful to return in a REPL.
 */
r5js.test.JsInterop.prototype['testNonStandardExternalRepresentations'] =
    function() {
  expect('+').not().to(haveStringValue(''));
  expect('(lambda (x) x)').not().to(haveStringValue(''));
  expect('(current-input-port)').not().to(haveStringValue(''));
  expect('(current-output-port)').not().to(haveStringValue(''));
  expect('(scheme-report-environment 5)').not().to(haveStringValue(''));
  expect('(null-environment 5)').not().to(haveStringValue(''));
};


r5js.test.JsInterop.prototype['testUnspecifiedReturnValues'] = function() {
  expect('').to(haveJsValue(undefined));
  expect('').to(haveStringValue(''));
  expect(' ').to(haveJsValue(undefined));
  expect(' ').to(haveStringValue(''));
  expect('\n').to(haveJsValue(undefined));
  expect('\n').to(haveStringValue(''));
  expect('\t').to(haveJsValue(undefined));
  expect('\t').to(haveStringValue(''));
  expect('    \t \n\n\n   ').to(haveJsValue(undefined));
  expect('    \t \n\n\n   ').to(haveStringValue(''));
  expect('(define x 1)').to(haveJsValue(undefined));
  expect('(define x 1)').to(haveStringValue(''));
  expect('(define x 1) (set! x 2)').to(haveJsValue(undefined));
  expect('(define x 1) (set! x 2)').to(haveStringValue(''));
  expect('(define x (cons 1 2)) (set-car! x x)').to(haveJsValue(undefined));
  expect('(define x (cons 1 2)) (set-car! x x)').to(haveStringValue(''));
  expect('(define x (cons 1 2)) (set-cdr! x x)').to(haveJsValue(undefined));
  expect('(define x (cons 1 2)) (set-cdr! x x)').to(haveStringValue(''));
  expect('(if #f #t)').to(haveJsValue(undefined));
  expect('(if #f #t)').to(haveStringValue(''));
  expect('(write "foo")').to(haveJsValue(undefined));
  expect('(write "foo")').to(haveStringValue(''));
  expect('(display 42)').to(haveJsValue(undefined));
  expect('(display 42)').to(haveStringValue(''));
  expect('(write-char #\\a)').to(haveJsValue(undefined));
  expect('(write-char #\\a)').to(haveStringValue(''));
  expect('(close-input-port (current-input-port))').
      to(haveJsValue(undefined));
  expect('(close-input-port (current-input-port))').to(haveStringValue(''));
  expect('(close-input-port (open-input-file "foo"))').
      to(haveJsValue(undefined));
  expect('(close-input-port (open-input-file "foo"))').to(haveStringValue(''));
  expect('(close-output-port (open-output-file "foo"))').
      to(haveJsValue(undefined));
  expect('(close-output-port (open-output-file "foo"))').
      to(haveStringValue(''));
  expect('(close-output-port (current-output-port))').
      to(haveJsValue(undefined));
  expect('(close-output-port (current-output-port))').to(haveStringValue(''));
};

r5js.test.JsInterop.prototype['testErrors'] = function() {
  expect('(').to(Throw2(new r5js.ReadError(r5js.parse.Terminals.LPAREN)));
  expect(')').to(Throw2(new r5js.ReadError(r5js.parse.Terminals.RPAREN)));
  expect('(eval)').to(Throw2(new r5js.IncorrectNumArgs('eval', 2, 0)));
  expect('(eval 1 2 3 4 5)').
      to(Throw2(new r5js.IncorrectNumArgs('eval', 2, 5)));
  expect('(let ((foo (lambda (x) x))) (foo))').
      to(Throw2(new r5js.IncorrectNumArgs(''/* TODO bl lambda */, 1, 0)));
  expect('(let ((foo (lambda (x) x))) (foo 1 2))').
      to(Throw2(new r5js.IncorrectNumArgs('' /* TODO bl lambda */, 1, 2)));
  expect("(set-car! '(1 2 3) 4)").to(Throw2(new r5js.ImmutableError('')));
  expect('(let ((g (lambda () "***"))) (string-set! (g) 0 #\\?))').
      to(Throw2(new r5js.ImmutableError(''))); // Example from R5RS 6.3.5
  expect("(string-set! (symbol->string 'immutable) 0 #\\?)").
      to(Throw2(new r5js.ImmutableError(''))); // Example from R5RS 6.3.5
  expect("(vector-set! '#(0 1 2) 1 \"doe\")").
      to(Throw2(new r5js.ImmutableError(''))); // Example from R5RS 6.3.6
  expect('(make-vector)').
      to(Throw2(new r5js.TooFewVarargs('make-vector', 1, 0)));
  expect('(make-vector 1 2 3 4 5)').
      to(Throw2(new r5js.TooManyVarargs('make-vector', 2, 5)));
  expect('(let ((foo (lambda (x . y) x))) (foo))').
      to(Throw2(new r5js.TooFewVarargs('', 1, 0)));
  expect('(+ "a" "b")').
      to(Throw2(new r5js.ArgumentTypeError(
          'a', 0, '+', r5js.DatumType.NUMBER, r5js.DatumType.STRING)));
  expect('(scheme-report-environment 6)').
      to(Throw2(new r5js.UnimplementedOptionError('')));
  expect('(null-environment 6)').
      to(Throw2(new r5js.UnimplementedOptionError('')));
};

