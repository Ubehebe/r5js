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


goog.require('Throw');
goog.require('expect');
goog.require('goog.Promise');
goog.require('goog.functions');
goog.require('goog.string');
goog.require('goog.testing.asserts');
goog.require('haveJsOutput');
goog.require('haveJsValue');
goog.require('haveStringOutput');
goog.require('haveStringValue');
goog.require('r5js.DatumType');
goog.require('r5js.error');
goog.require('r5js.parse.Terminals');
goog.require('r5js.test.SyncPromiseTestSuite');
goog.require('r5js.test.matchers.setOutputPort');
goog.require('tdd.TestType');



/**
 * Tests exercising Scheme->JavaScript interoperability.
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.OutputSavingPort} outputPort
 * @extends {r5js.test.SyncPromiseTestSuite}
 * @constructor
 */
r5js.test.JsInterop = function(evaluator, outputPort) {
  goog.base(this, 'r5js.test.JsInterop');
  /** @const @private */ this.evaluator_ = evaluator;
  /** @const @private */ this.outputPort_ = outputPort;
  r5js.test.matchers.setOutputPort(outputPort);
};
goog.inherits(r5js.test.JsInterop, r5js.test.SyncPromiseTestSuite);


/** @override */
r5js.test.JsInterop.prototype.expect = function(input) {
  /* TODO bl: without the goog.functions.identity callback,
    rejected evaluation promises end up invoking goog.promise's
    unhandled rejection handler. By default, the unhandled rejection handler
    throws an exception on the next tick, which is fine for browsers
    (it ends up on the console) but terminates execution for Node.

    After auditing all my goog.Promise#then call sites, I haven't been able
    to figure out why the unhandled rejection handler is getting called.
    It seems that goog.promise.removeUnhandledRejection_ is getting called
    after goog.promise.addUnhandledRejection_, by which time it is too late. */
  return goog.base(this, 'expect', input,
      this.evaluator_.evaluate(input).thenCatch(goog.functions.identity));
};


r5js.test.JsInterop.prototype['testReturnPrimitivesToJs'] = function() {
  this.expect('42').to(haveJsValue(42));
  this.expect('42').to(haveStringValue('42'));
  this.expect('#t').to(haveJsValue(true));
  this.expect('42').to(haveJsValue(42));
  this.expect('42').to(haveStringValue('42'));
  this.expect('#t').to(haveJsValue(true));
  this.expect('#t').to(haveStringValue('#t'));
  this.expect('#f').to(haveJsValue(false));
  this.expect('#f').to(haveStringValue('#f'));
  this.expect('"hello, world"').to(haveJsValue('hello, world'));
  this.expect('"hello, world"').to(haveStringValue('"hello, world"'));
  this.expect("'hello").to(haveJsValue('hello'));
  this.expect("'hello").to(haveStringValue('hello'));
  this.expect('(quote hello)').to(haveJsValue('hello'));
  this.expect('(quote hello)').to(haveStringValue('hello'));
  this.expect('#\\a').to(haveJsValue('a'));
  this.expect('#\\a').to(haveStringValue('#\\a'));
  this.expect('#\\space').to(haveJsValue(' '));
  this.expect('#\\space').to(haveStringValue('#\\space'));
  this.expect('#\\newline').to(haveJsValue('\n'));
  this.expect('#\\newline').to(haveStringValue('#\\newline'));
};


//r5js.test.JsInterop.prototype['testDisplayPrimitivesToJs'] = function() {
//  this.expect('(display 42)').to(haveJsOutput(42));
//      this.expect('(display 42)').to(haveStringOutput('42'));
//      this.expect('(display #t)').to(haveJsOutput(true));
//      this.expect('(display #t)').to(haveStringOutput('#t'));
//      this.expect('(display #f)').to(haveJsOutput(false));
//      this.expect('(display #f)').to(haveStringOutput('#t'));
//      expect('(display "hello, world")', haveJsOutput('hello, world')).
//      expect('(display "hello, world")', haveStringOutput('hello, world')).
//      this.expect("(display 'hello)").to(haveJsOutput('hello'));
//      this.expect("(display 'hello)").to(haveStringOutput('hello'));
//      this.expect('(display (quote hello))').to(haveJsOutput('hello'));
//      this.expect('(display (quote hello))').to(haveStringOutput('hello'));
//      this.expect('(display #\\a)').to(haveJsOutput('a'));
//      this.expect('(display #\\a)').to(haveStringOutput('a'));
//      this.expect('(display #\\space)').to(haveJsOutput(' '));
//      this.expect('(display #\\space)').to(haveStringOutput(' '));
//      this.expect('(display #\\newline)').to(haveJsOutput('\n'));
//      this.expect('(display #\\newline)').to(haveStringOutput('\n'));
//};


//r5js.test.JsInterop.prototype['testWritePrimitivesToJs'] = function() {
//  return this.expect('(write 42)', haveJsOutput(42)).
//      this.expect('(write 42)').to(haveStringOutput('42'));
//      this.expect('(write #t)').to(haveJsOutput(true));
//      this.expect('(write #t)').to(haveStringOutput('#t'));
//      this.expect('(write #f)').to(haveJsOutput(false));
//      this.expect('(write #f)').to(haveStringOutput('#f'));
//      expect('(write "hello, world")', haveJsOutput('hello, world')).
//  expect('(write "hello, world")', haveStringOutput('"hello, world"')).
//      this.expect("(write 'hello)").to(haveJsOutput('hello'));
//      this.expect("(write 'hello)").to(haveStringOutput('hello'));
//      this.expect('(write (quote hello))').to(haveJsOutput('hello'));
//      this.expect('(write (quote hello))').to(haveStringOutput('hello'));
//      this.expect('(write #\\a)').to(haveJsOutput('a'));
//      //  this.expect('(write #\\a)').to(haveStringOutput('#\\a'));
//      this.expect('(write #\\space)').to(haveJsOutput(' '));
//      //  this.expect('(write #\\space)').to(haveStringOutput('#\\space'));
//      this.expect('(write #\\newline)').to(haveJsOutput('\n'));done();
//  //  this.expect('(write #\\newline)').to(haveStringOutput('#\\newline'));
//};


r5js.test.JsInterop.prototype['testSanityChecks'] = function() {
  this.expect('(+ 1 1)').to(haveJsValue(2));
  this.expect('(+ 1 1)').to(haveStringValue('2'));
  this.expect('(procedure? procedure?)').to(haveJsValue(true));
  this.expect('(procedure? procedure?)').to(haveStringValue('#t'));
  this.expect('(string-append "hello " "world")').
      to(haveJsValue('hello world'));
  this.expect('(string-append "hello " "world")').
      to(haveStringValue('"hello world"'));
  this.expect("'a").to(haveStringValue('a'));
  this.expect("''a").to(haveStringValue("'a"));
  this.expect("'''a").to(haveStringValue("''a"));
  this.expect("''''a").to(haveStringValue("'''a"));
  this.expect("'''''a").to(haveStringValue("''''a"));
};


r5js.test.JsInterop.prototype['testReturnRecursiveTypesToJs'] = function() {
  this.expect('#()').to(haveJsValue([]));
  this.expect('#()').to(haveStringValue('#()'));
  this.expect("'()").to(haveJsValue([]));
  this.expect("'()").to(haveStringValue('()'));
  this.expect("(list '() '() '() '(42))").to(haveJsValue([[], [], [], [42]]));
  this.expect("(list '() '() '() '(42))").
      to(haveStringValue('(() () () (42))'));
  this.expect('(list 1 2 3)').to(haveJsValue([1, 2, 3]));
  this.expect('(list 1 2 3)').to(haveStringValue('(1 2 3)'));
  this.expect("(cons 'a (cons 'b (cons 'c '())))").
      to(haveJsValue(['a', 'b', 'c']));
  this.expect("(cons 'a (cons 'b (cons 'c '())))").
      to(haveStringValue('(a b c)'));
  this.expect("(cons 'a 'b)").not().to(haveJsValue(['a', 'b']));
  this.expect("(cons 'a 'b)").to(haveStringValue('(a . b)'));
};


//r5js.test.JsInterop.prototype['testDisplayRecursiveTypesToJs'] = function() {
//  return this.expect('(display #())', haveJsOutput([])).
//      this.expect('(display #())').to(haveStringOutput('#()'));
//      this.expect("(display '())").to(haveJsOutput([]));
//      this.expect("(display '())").to(haveStringOutput('()'));
//      expect("(display (list '() '() '() '(42)))",
//      haveJsOutput([[], [], [], [42]])).
//      expect("(display (list '() '() '() '(42)))",
//          haveStringOutput('(() () () (42))')).
//      expect('(display (list 1 2 3))', haveJsOutput([1, 2, 3])).
//      this.expect('(display (list 1 2 3))').to(haveStringOutput('(1 2 3)'));
//      expect("(display (cons 'a (cons 'b (cons 'c '()))))",
//      haveJsOutput(['a', 'b', 'c'])).
//      expect("(display (cons 'a (cons 'b (cons 'c '()))))",
//          haveStringOutput('(a b c)')).
//      this.expect("(display (cons 'a 'b))").to(haveStringOutput('(a . b)'));
//};


//r5js.test.JsInterop.prototype['testWriteRecursiveTypesToJs'] = function() {
//  return this.expect('(write #())', haveJsOutput([])).
//      this.expect('(write #())').to(haveStringOutput('#()'));
//      this.expect("(write '())").to(haveJsOutput([]));
//      this.expect("(write '())").to(haveStringOutput('()'));
//      expect("(write (list '() '() '() '(42)))",
//      haveJsOutput([[], [], [], [42]])).
//      expect("(write (list '() '() '() '(42)))",
//          haveStringOutput('(() () () (42))')).
//      expect('(write (list 1 2 3))', haveJsOutput([1, 2, 3])).
//      this.expect('(write (list 1 2 3))').to(haveStringOutput('(1 2 3)'));
//      expect("(write (cons 'a (cons 'b (cons 'c '()))))",
//      haveJsOutput(['a', 'b', 'c'])).
//      expect("(write (cons 'a (cons 'b (cons 'c '()))))",
//          haveStringOutput('(a b c)')).
//      //  expect("(write (cons 'a 'b))").not().to(haveJsOutput(['a', 'b']));
//      this.expect("(write (cons 'a 'b))").to(haveStringOutput('(a . b)'));
//};


/*
 * R5RS doesn't actually forbid these external representations to be
 * the empty string, but empty strings are not helpful to return in a REPL.
 */
r5js.test.JsInterop.prototype['testNonStandardExternalRepresentations'] =
    function() {
  this.expect('+').not().to(haveStringValue(''));
  this.expect('(lambda (x) x)').not().to(haveStringValue(''));
  this.expect('(current-input-port)').not().to(haveStringValue(''));
  this.expect('(current-output-port)').not().to(haveStringValue(''));
  this.expect('(scheme-report-environment 5)').
      not().to(haveStringValue(''));
  this.expect('(null-environment 5)').not().to(haveStringValue(''));
};


r5js.test.JsInterop.prototype['testUnspecifiedReturnValues'] = function() {
  this.expect('').to(haveJsValue(undefined));
  this.expect('').to(haveStringValue(''));
  this.expect(' ').to(haveJsValue(undefined));
  this.expect(' ').to(haveStringValue(''));
  this.expect('\n').to(haveJsValue(undefined));
  this.expect('\n').to(haveStringValue(''));
  this.expect('\t').to(haveJsValue(undefined));
  this.expect('\t').to(haveStringValue(''));
  this.expect('    \t \n\n\n   ').to(haveJsValue(undefined));
  this.expect('    \t \n\n\n   ').to(haveStringValue(''));
  this.expect('(define x 1)').to(haveJsValue(undefined));
  this.expect('(define x 1)').to(haveStringValue(''));
  this.expect('(define x 1) (set! x 2)').to(haveJsValue(undefined));
  this.expect('(define x 1) (set! x 2)').to(haveStringValue(''));
  this.expect('(define x (cons 1 2)) (set-car! x x)').
      to(haveJsValue(undefined));
  this.expect('(define x (cons 1 2)) (set-car! x x)').to(haveStringValue(''));
  this.expect('(define x (cons 1 2)) (set-cdr! x x)').
      to(haveJsValue(undefined));
  this.expect('(define x (cons 1 2)) (set-cdr! x x)').to(haveStringValue(''));
  this.expect('(if #f #t)').to(haveJsValue(undefined));
  this.expect('(if #f #t)').to(haveStringValue(''));
  this.expect('(write "foo")').to(haveJsValue(undefined));
  this.expect('(write "foo")').to(haveStringValue(''));
  this.expect('(display 42)').to(haveJsValue(undefined));
  this.expect('(display 42)').to(haveStringValue(''));
  this.expect('(write-char #\\a)').to(haveJsValue(undefined));
  this.expect('(write-char #\\a)').to(haveStringValue(''));
  this.expect('(close-input-port (current-input-port))').
      to(haveJsValue(undefined));
  this.expect('(close-input-port (current-input-port))').
      to(haveStringValue(''));
  this.expect('(close-input-port (open-input-file "foo"))').
      to(haveJsValue(undefined));
  this.expect('(close-input-port (open-input-file "foo"))').
      to(haveStringValue(''));
  this.expect('(close-output-port (open-output-file "foo"))').
      to(haveJsValue(undefined));
  this.expect('(close-output-port (open-output-file "foo"))').
      to(haveStringValue(''));
  this.expect('(close-output-port (current-output-port))').
      to(haveJsValue(undefined));
  this.expect('(close-output-port (current-output-port))').
      to(haveStringValue(''));
};

r5js.test.JsInterop.prototype['testErrors'] = function() {
  this.expect('(').to(Throw(r5js.error.read(r5js.parse.Terminals.LPAREN)));
  this.expect(')').to(Throw(r5js.error.read(r5js.parse.Terminals.RPAREN)));
  this.expect('(eval)').to(Throw(r5js.error.incorrectNumArgs('eval', 2, 0)));
  this.expect('(eval 1 2 3 4 5)').
      to(Throw(r5js.error.incorrectNumArgs('eval', 2, 5)));
  this.expect('(let ((foo (lambda (x) x))) (foo))').
      to(Throw(r5js.error.incorrectNumArgs(''/* TODO bl lambda */, 1, 0)));
  this.expect('(let ((foo (lambda (x) x))) (foo 1 2))').
      to(Throw(r5js.error.incorrectNumArgs('' /* TODO bl lambda */, 1, 2)));
  this.expect("(set-car! '(1 2 3) 4)").to(Throw(r5js.error.immutable('')));
  this.expect('(let ((g (lambda () "***"))) (string-set! (g) 0 #\\?))').
      to(Throw(r5js.error.immutable(''))); // Example from R5RS 6.3.5
  this.expect("(string-set! (symbol->string 'immutable) 0 #\\?)").
      to(Throw(r5js.error.immutable(''))); // Example from R5RS 6.3.5
  this.expect("(vector-set! '#(0 1 2) 1 \"doe\")").
      to(Throw(r5js.error.immutable(''))); // Example from R5RS 6.3.6
  this.expect('(make-vector)').
      to(Throw(r5js.error.tooFewVarargs('make-vector', 1, 0)));
  this.expect('(make-vector 1 2 3 4 5)').
      to(Throw(r5js.error.tooManyVarargs('make-vector', 2, 5)));
  this.expect('(let ((foo (lambda (x . y) x))) (foo))').
      to(Throw(r5js.error.tooFewVarargs('', 1, 0)));
  this.expect('(+ "a" "b")').
      to(Throw(r5js.error.argumentTypeError(
          'a', 0, '+', r5js.DatumType.NUMBER, r5js.DatumType.STRING)));
  this.expect('(scheme-report-environment 6)').
      to(Throw(r5js.error.unimplementedOption('')));
  this.expect('(null-environment 6)').
      to(Throw(r5js.error.unimplementedOption('')));
};

