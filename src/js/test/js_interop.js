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
goog.require('evalTo');
goog.require('goog.testing.asserts');
goog.require('output');
goog.require('r5js.Error');
goog.require('r5js.Type');
goog.require('r5js.parse.Terminals');
goog.require('r5js.test.SyncPromiseTestSuite');
goog.require('r5js.test.matchers.setOutputPort');



/**
 * Tests exercising Scheme->JavaScript interoperability.
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.OutputSavingPort} outputPort
 * @extends {r5js.test.SyncPromiseTestSuite}
 * @constructor
 */
r5js.test.JsInterop = function(evaluator, outputPort) {
  r5js.test.JsInterop.base(this, 'constructor', 'r5js.test.JsInterop');
  /** @const @private */ this.evaluator_ = evaluator;
  /** @const @private */ this.outputPort_ = outputPort;
  r5js.test.matchers.setOutputPort(outputPort);
};
goog.inherits(r5js.test.JsInterop, r5js.test.SyncPromiseTestSuite);


/** @override */
r5js.test.JsInterop.prototype.expect = function(input) {
  return r5js.test.JsInterop.base(
      this, 'expect', input, this.evaluator_.evaluate(input));
};


/** @override */
r5js.test.JsInterop.prototype.estimateSize = function() {
  return 10; // TODO bl manually count
};


r5js.test.JsInterop.prototype['testReturnPrimitivesToJs'] = function() {
  this.expect('42').to(evalTo('42'));
  this.expect('42').to(evalTo('42'));
  this.expect('#t').to(evalTo('#t'));
  this.expect('#f').to(evalTo('#f'));
  this.expect('"hello, world"').to(evalTo('"hello, world"'));
  this.expect("'hello").to(evalTo('hello'));
  this.expect('(quote hello)').to(evalTo('hello'));
  this.expect('#\\a').to(evalTo('#\\a'));
  this.expect('#\\space').to(evalTo('#\\space'));
  this.expect('#\\newline').to(evalTo('#\\newline'));
};


r5js.test.JsInterop.prototype['testDisplayPrimitivesToJs'] = function() {
  this.expect('(display 42)').to(output('42'));
  this.expect('(display #t)').to(output('#t'));
  this.expect('(display #f)').to(output('#f'));
  this.expect('(display "hello, world")').to(output('hello, world'));
  this.expect("(display 'hello)").to(output('hello'));
  this.expect('(display (quote hello))').to(output('hello'));
  this.expect('(display #\\a)').to(output('a'));
  this.expect('(display #\\space)').to(output(' '));
  this.expect('(display #\\newline)').to(output('\n'));
};


r5js.test.JsInterop.prototype['testWritePrimitivesToJs'] = function() {
  this.expect('(write 42)').to(output('42'));
  this.expect('(write #t)').to(output('#t'));
  this.expect('(write #f)').to(output('#f'));
  this.expect('(write "hello, world")').to(output('"hello, world"'));
  this.expect("(write 'hello)").to(output('hello'));
  this.expect('(write (quote hello))').to(output('hello'));
  this.expect('(write #\\a)').to(output('#\\a'));
  this.expect('(write #\\space)').to(output('#\\space'));
  this.expect('(write #\\newline)').to(output('#\\newline'));
};


r5js.test.JsInterop.prototype['testSanityChecks'] = function() {
  this.expect('(+ 1 1)').to(evalTo('2'));
  this.expect('(procedure? procedure?)').to(evalTo('#t'));
  this.expect('(string-append "hello " "world")').
      to(evalTo('"hello world"'));
  this.expect("'a").to(evalTo('a'));
  this.expect("''a").to(evalTo("'a"));
  this.expect("'''a").to(evalTo("''a"));
  this.expect("''''a").to(evalTo("'''a"));
  this.expect("'''''a").to(evalTo("''''a"));
};


r5js.test.JsInterop.prototype['testReturnRecursiveTypesToJs'] = function() {
  this.expect('#()').to(evalTo('#()'));
  this.expect("'()").to(evalTo('()'));
  this.expect("(list '() '() '() '(42))").
      to(evalTo('(() () () (42))'));
  this.expect('(list 1 2 3)').to(evalTo('(1 2 3)'));
  this.expect("(cons 'a (cons 'b (cons 'c '())))").
      to(evalTo('(a b c)'));
  this.expect("(cons 'a 'b)").to(evalTo('(a . b)'));
};


r5js.test.JsInterop.prototype['testDisplayRecursiveTypesToJs'] = function() {
  this.expect('(display #())').to(output('#()'));
  this.expect("(display '())").to(output('()'));
  this.expect("(display (list '() '() '() '(42)))").
      to(output('(() () () (42))'));
  this.expect('(display (list 1 2 3))').to(output('(1 2 3)'));
  this.expect("(display (cons 'a (cons 'b (cons 'c '()))))").
      to(output('(a b c)'));
  this.expect("(display (cons 'a 'b))").to(output('(a . b)'));
};


r5js.test.JsInterop.prototype['testWriteRecursiveTypesToJs'] = function() {
  this.expect('(write #())').to(output('#()'));
  this.expect("(write '())").to(output('()'));
  this.expect("(write (list '() '() '() '(42)))").to(output('(() () () (42))'));
  this.expect('(write (list 1 2 3))').to(output('(1 2 3)'));
  this.expect("(write (cons 'a (cons 'b (cons 'c '()))))").
      to(output('(a b c)'));
  this.expect("(write (cons 'a 'b))").to(output('(a . b)'));
};


/*
 * R5RS doesn't actually forbid these external representations to be
 * the empty string, but empty strings are not helpful to return in a REPL.
 */
r5js.test.JsInterop.prototype['testNonStandardExternalRepresentations'] =
    function() {
  this.expect('+').not().to(evalTo(''));
  this.expect('(lambda (x) x)').not().to(evalTo(''));
  this.expect('(current-input-port)').not().to(evalTo(''));
  this.expect('(current-output-port)').not().to(evalTo(''));
  this.expect('(scheme-report-environment 5)').
      not().to(evalTo(''));
  this.expect('(null-environment 5)').not().to(evalTo(''));
};


r5js.test.JsInterop.prototype['testUnspecifiedReturnValues'] = function() {
  this.expect('').to(evalTo(''));
  this.expect(' ').to(evalTo(''));
  this.expect('\n').to(evalTo(''));
  this.expect('\t').to(evalTo(''));
  this.expect('    \t \n\n\n   ').to(evalTo(''));
  this.expect('(define x 1)').to(evalTo(''));
  this.expect('(define x 1) (set! x 2)').to(evalTo(''));
  this.expect('(define x (cons 1 2)) (set-car! x x)').to(evalTo(''));
  this.expect('(define x (cons 1 2)) (set-cdr! x x)').to(evalTo(''));
  this.expect('(if #f #t)').to(evalTo(''));
  this.expect('(write "foo")').to(evalTo(''));
  this.expect('(display 42)').to(evalTo(''));
  this.expect('(write-char #\\a)').to(evalTo(''));
  this.expect('(close-input-port (current-input-port))').
      to(evalTo(''));
  this.expect('(close-input-port (open-input-file "foo"))').
      to(evalTo(''));
  this.expect('(close-output-port (open-output-file "foo"))').
      to(evalTo(''));
  this.expect('(close-output-port (current-output-port))').
      to(evalTo(''));
};

r5js.test.JsInterop.prototype['testErrors'] = function() {
  this.expect('(').to(Throw(r5js.Error.read(r5js.parse.Terminals.LPAREN)));
  this.expect(')').to(Throw(r5js.Error.read(r5js.parse.Terminals.RPAREN)));
  this.expect('(eval)').to(Throw(r5js.Error.incorrectNumArgs('eval', 2, 0)));
  this.expect('(eval 1 2 3 4 5)').
      to(Throw(r5js.Error.incorrectNumArgs('eval', 2, 5)));
  this.expect('(let ((foo (lambda (x) x))) (foo))').
      to(Throw(r5js.Error.incorrectNumArgs(''/* TODO bl lambda */, 1, 0)));
  this.expect('(let ((foo (lambda (x) x))) (foo 1 2))').
      to(Throw(r5js.Error.incorrectNumArgs('' /* TODO bl lambda */, 1, 2)));
  this.expect("(set-car! '(1 2 3) 4)").to(Throw(r5js.Error.immutable('')));
  this.expect('(let ((g (lambda () "***"))) (string-set! (g) 0 #\\?))').
      to(Throw(r5js.Error.immutable(''))); // Example from R5RS 6.3.5
  this.expect("(string-set! (symbol->string 'immutable) 0 #\\?)").
      to(Throw(r5js.Error.immutable(''))); // Example from R5RS 6.3.5
  this.expect("(vector-set! '#(0 1 2) 1 \"doe\")").
      to(Throw(r5js.Error.immutable(''))); // Example from R5RS 6.3.6
  this.expect('(make-vector)').
      to(Throw(r5js.Error.tooFewVarargs('make-vector', 1, 0)));
  this.expect('(make-vector 1 2 3 4 5)').
      to(Throw(r5js.Error.tooManyVarargs('make-vector', 2, 5)));
  this.expect('(let ((foo (lambda (x . y) x))) (foo))').
      to(Throw(r5js.Error.tooFewVarargs('', 1, 0)));
  this.expect('(+ "a" "b")').
      to(Throw(r5js.Error.argumentTypeError(
          'a', 0, '+', r5js.Type.Types.NUMBER, r5js.Type.Types.STRING)));
  this.expect('(scheme-report-environment 6)').
      to(Throw(r5js.Error.unimplementedOption('')));
  this.expect('(null-environment 6)').
      to(Throw(r5js.Error.unimplementedOption('')));
};

