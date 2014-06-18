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
goog.require('goog.testing.asserts');
goog.require('haveJsOutput');
goog.require('haveJsValue');
goog.require('haveStringOutput');
goog.require('haveStringValue');
goog.require('r5js.ArgumentTypeError');
goog.require('r5js.DatumType');
goog.require('r5js.ImmutableError');
goog.require('r5js.IncorrectNumArgs');
goog.require('r5js.ReadError');
goog.require('r5js.UnimplementedOptionError');
goog.require('r5js.parse.Terminals');
goog.require('r5js.test.matchers.setOutputPort');
goog.require('tdd.TestType');



/**
 * Tests exercising Scheme->JavaScript interoperability.
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.OutputSavingPort} outputPort
 * @implements {tdd.TestSuite}
 * @struct
 * @constructor
 */
r5js.test.JsInterop = function(evaluator, outputPort) {
  /** @const @private */ this.evaluator_ = evaluator;
  /** @const @private */ this.outputPort_ = outputPort;
  r5js.test.matchers.setOutputPort(outputPort);
  this.promise_ = goog.Promise.resolve();
};


/** @override */
r5js.test.JsInterop.prototype.getType = goog.functions.constant(
    tdd.TestType.UNIT);


/** @override */
r5js.test.JsInterop.prototype.toString = goog.functions.constant(
    'r5js.test.JsInterop');


/**
 * @param {string} input
 * @param {!tdd.matchers.Matcher} matcher
 * @return {!r5js.test.JsInterop} This object, for chaining.
 */
r5js.test.JsInterop.prototype.expect = function(input, matcher) {
  this.promise_ = this.promise_.then(function() {
    return this.evaluator_.evaluate(input);
  }, undefined /* opt_onRejected */, this).then(function(value) {
    if (!matcher.matches(value)) {
      throw new Error(matcher.getFailureMessage(value));
    }
  });
  return this;
};


/** @return {!goog.Promise} */
r5js.test.JsInterop.prototype.done = function() {
  var promise = this.promise_;
  this.promise_ = goog.Promise.resolve();
  return promise;
};


r5js.test.JsInterop.prototype['testReturnPrimitivesToJs'] = function() {
  return this.expect('42', haveJsValue(42)).
      expect('42', haveStringValue('42')).
      expect('#t', haveJsValue(true)).
      expect('42', haveJsValue(42)).
      expect('42', haveStringValue('42')).
      expect('#t', haveJsValue(true)).
      expect('#t', haveStringValue('#t')).
      expect('#f', haveJsValue(false)).
      expect('#f', haveStringValue('#f')).
      expect('"hello, world"', haveJsValue('hello, world')).
      expect('"hello, world"', haveStringValue('"hello, world"')).
      expect("'hello", haveJsValue('hello')).
      expect("'hello", haveStringValue('hello')).
      expect('(quote hello)', haveJsValue('hello')).
      expect('(quote hello)', haveStringValue('hello')).
      expect('#\\a', haveJsValue('a')).
      expect('#\\a', haveStringValue('#\\a')).
      expect('#\\space', haveJsValue(' ')).
      expect('#\\space', haveStringValue('#\\space')).
      expect('#\\newline', haveJsValue('\n')).
      expect('#\\newline', haveStringValue('#\\newline')).done();
};


r5js.test.JsInterop.prototype['testDisplayPrimitivesToJs'] = function() {
  return this.expect('(display 42)', haveJsOutput(42)).
      expect('(display 42)', haveStringOutput('42')).
      expect('(display #t)', haveJsOutput(true)).
      expect('(display #t)', haveStringOutput('#t')).
      expect('(display #f)', haveJsOutput(false)).
      expect('(display #f)', haveStringOutput('#f')).
      expect('(display "hello, world")', haveJsOutput('hello, world')).
      expect('(display "hello, world")', haveStringOutput('hello, world')).
      expect("(display 'hello)", haveJsOutput('hello')).
      expect("(display 'hello)", haveStringOutput('hello')).
      expect('(display (quote hello))', haveJsOutput('hello')).
      expect('(display (quote hello))', haveStringOutput('hello')).
      expect('(display #\\a)', haveJsOutput('a')).
      expect('(display #\\a)', haveStringOutput('a')).
      expect('(display #\\space)', haveJsOutput(' ')).
      expect('(display #\\space)', haveStringOutput(' ')).
      expect('(display #\\newline)', haveJsOutput('\n')).
      expect('(display #\\newline)', haveStringOutput('\n')).done();
};


r5js.test.JsInterop.prototype['testWritePrimitivesToJs'] = function() {
  return this.expect('(write 42)', haveJsOutput(42)).
      expect('(write 42)', haveStringOutput('42')).
      expect('(write #t)', haveJsOutput(true)).
      expect('(write #t)', haveStringOutput('#t')).
      expect('(write #f)', haveJsOutput(false)).
      expect('(write #f)', haveStringOutput('#f')).
      expect('(write "hello, world")', haveJsOutput('hello, world')).
      //  expect('(write "hello, world")', haveStringOutput('"hello, world"')).
      expect("(write 'hello)", haveJsOutput('hello')).
      expect("(write 'hello)", haveStringOutput('hello')).
      expect('(write (quote hello))', haveJsOutput('hello')).
      expect('(write (quote hello))', haveStringOutput('hello')).
      expect('(write #\\a)', haveJsOutput('a')).
      //  expect('(write #\\a)', haveStringOutput('#\\a')).
      expect('(write #\\space)', haveJsOutput(' ')).
      //  expect('(write #\\space)', haveStringOutput('#\\space')).
      expect('(write #\\newline)', haveJsOutput('\n')).done();
  //  expect('(write #\\newline)', haveStringOutput('#\\newline')).done();
};


r5js.test.JsInterop.prototype['testSanityChecks'] = function() {
  return this.expect('(+ 1 1)', haveJsValue(2)).
      expect('(+ 1 1)', haveStringValue('2')).
      expect('(procedure? procedure?)', haveJsValue(true)).
      expect('(procedure? procedure?)', haveStringValue('#t')).
      expect('(string-append "hello " "world")', haveJsValue('hello world')).
      expect('(string-append "hello " "world")',
              haveStringValue('"hello world"')).
      expect("'a", haveStringValue('a')).
      expect("''a", haveStringValue("'a")).
      expect("'''a", haveStringValue("''a")).
      expect("''''a", haveStringValue("'''a")).
      expect("'''''a", haveStringValue("''''a")).done();
};


r5js.test.JsInterop.prototype['testReturnRecursiveTypesToJs'] = function() {
  return this.expect('#()', haveJsValue([])).
      expect('#()', haveStringValue('#()')).
      expect("'()", haveJsValue([])).
      expect("'()", haveStringValue('()')).
      expect("(list '() '() '() '(42))", haveJsValue([[], [], [], [42]])).
      expect("(list '() '() '() '(42))", haveStringValue('(() () () (42))')).
      expect('(list 1 2 3)', haveJsValue([1, 2, 3])).
      expect('(list 1 2 3)', haveStringValue('(1 2 3)')).
      expect("(cons 'a (cons 'b (cons 'c '())))", haveJsValue(['a', 'b', 'c'])).
      expect("(cons 'a (cons 'b (cons 'c '())))", haveStringValue('(a b c)')).
      //    expect("(cons 'a 'b)").not().to(haveJsValue(['a', 'b'])).done();
      expect("(cons 'a 'b)", haveStringValue('(a . b)')).done();
};


r5js.test.JsInterop.prototype['testDisplayRecursiveTypesToJs'] = function() {
  return this.expect('(display #())', haveJsOutput([])).
      expect('(display #())', haveStringOutput('#()')).
      expect("(display '())", haveJsOutput([])).
      expect("(display '())", haveStringOutput('()')).
      expect("(display (list '() '() '() '(42)))",
      haveJsOutput([[], [], [], [42]])).
      expect("(display (list '() '() '() '(42)))",
          haveStringOutput('(() () () (42))')).
      expect('(display (list 1 2 3))', haveJsOutput([1, 2, 3])).
      expect('(display (list 1 2 3))', haveStringOutput('(1 2 3)')).
      expect("(display (cons 'a (cons 'b (cons 'c '()))))",
      haveJsOutput(['a', 'b', 'c'])).
      expect("(display (cons 'a (cons 'b (cons 'c '()))))",
          haveStringOutput('(a b c)')).
      expect("(display (cons 'a 'b))", haveStringOutput('(a . b)')).done();
};


r5js.test.JsInterop.prototype['testWriteRecursiveTypesToJs'] = function() {
  return this.expect('(write #())', haveJsOutput([])).
      expect('(write #())', haveStringOutput('#()')).
      expect("(write '())", haveJsOutput([])).
      expect("(write '())", haveStringOutput('()')).
      expect("(write (list '() '() '() '(42)))",
      haveJsOutput([[], [], [], [42]])).
      expect("(write (list '() '() '() '(42)))",
          haveStringOutput('(() () () (42))')).
      expect('(write (list 1 2 3))', haveJsOutput([1, 2, 3])).
      expect('(write (list 1 2 3))', haveStringOutput('(1 2 3)')).
      expect("(write (cons 'a (cons 'b (cons 'c '()))))",
      haveJsOutput(['a', 'b', 'c'])).
      expect("(write (cons 'a (cons 'b (cons 'c '()))))",
          haveStringOutput('(a b c)')).
      //  expect("(write (cons 'a 'b))").not().to(haveJsOutput(['a', 'b']));
      expect("(write (cons 'a 'b))", haveStringOutput('(a . b)')).done();
};


/*
 * R5RS doesn't actually forbid these external representations to be
 * the empty string, but empty strings are not helpful to return in a REPL.
 */
//r5js.test.JsInterop.prototype['testNonStandardExternalRepresentations'] =
//    function() {
//  expect('+').not().to(haveStringValue(''));
//  expect('(lambda (x) x)').not().to(haveStringValue(''));
//  expect('(current-input-port)').not().to(haveStringValue(''));
//  expect('(current-output-port)').not().to(haveStringValue(''));
//  expect('(scheme-report-environment 5)').not().to(haveStringValue(''));
//  expect('(null-environment 5)').not().to(haveStringValue(''));
//};


r5js.test.JsInterop.prototype['testUnspecifiedReturnValues'] = function() {
  return this.expect('', haveJsValue(undefined)).
      expect('', haveStringValue('')).
      expect(' ', haveJsValue(undefined)).
      expect(' ', haveStringValue('')).
      expect('\n', haveJsValue(undefined)).
      expect('\n', haveStringValue('')).
      expect('\t', haveJsValue(undefined)).
      expect('\t', haveStringValue('')).
      expect('    \t \n\n\n   ', haveJsValue(undefined)).
      expect('    \t \n\n\n   ', haveStringValue('')).
      expect('(define x 1)', haveJsValue(undefined)).
      expect('(define x 1)', haveStringValue('')).
      expect('(define x 1) (set! x 2)', haveJsValue(undefined)).
      expect('(define x 1) (set! x 2)', haveStringValue('')).
      expect('(define x (cons 1 2)) (set-car! x x)', haveJsValue(undefined)).
      expect('(define x (cons 1 2)) (set-car! x x)', haveStringValue('')).
      expect('(define x (cons 1 2)) (set-cdr! x x)', haveJsValue(undefined)).
      expect('(define x (cons 1 2)) (set-cdr! x x)', haveStringValue('')).
      expect('(if #f #t)', haveJsValue(undefined)).
      expect('(if #f #t)', haveStringValue('')).
      expect('(write "foo")', haveJsValue(undefined)).
      expect('(write "foo")', haveStringValue('')).
      expect('(display 42)', haveJsValue(undefined)).
      expect('(display 42)', haveStringValue('')).
      expect('(write-char #\\a)', haveJsValue(undefined)).
      expect('(write-char #\\a)', haveStringValue('')).
      expect('(close-input-port (current-input-port))', haveJsValue(undefined)).
      expect('(close-input-port (current-input-port))', haveStringValue('')).
      expect('(close-input-port (open-input-file "foo"))',
          haveJsValue(undefined)).
      expect('(close-input-port (open-input-file "foo"))', haveStringValue('')).
      expect('(close-output-port (open-output-file "foo"))',
          haveJsValue(undefined)).
      expect('(close-output-port (open-output-file "foo"))',
          haveStringValue('')).
      expect('(close-output-port (current-output-port))',
          haveJsValue(undefined)).
      expect('(close-output-port (current-output-port))', haveStringValue('')).
      done();
};

//r5js.test.JsInterop.prototype['testErrors'] = function() {
//  expect('(').to(Throw(new r5js.ReadError(r5js.parse.Terminals.LPAREN)));
//  expect(')').to(Throw(new r5js.ReadError(r5js.parse.Terminals.RPAREN)));
//  expect('(eval)').to(Throw(new r5js.IncorrectNumArgs('eval', 2, 0)));
//  expect('(eval 1 2 3 4 5)').
//      to(Throw(new r5js.IncorrectNumArgs('eval', 2, 5)));
//  expect('(let ((foo (lambda (x) x))) (foo))').
//      to(Throw(new r5js.IncorrectNumArgs(''/* TODO bl lambda */, 1, 0)));
//  expect('(let ((foo (lambda (x) x))) (foo 1 2))').
//      to(Throw(new r5js.IncorrectNumArgs('' /* TODO bl lambda */, 1, 2)));
//  expect("(set-car! '(1 2 3) 4)").to(Throw(new r5js.ImmutableError('')));
//  expect('(let ((g (lambda () "***"))) (string-set! (g) 0 #\\?))').
//      to(Throw(new r5js.ImmutableError(''))); // Example from R5RS 6.3.5
//  expect("(string-set! (symbol->string 'immutable) 0 #\\?)").
//      to(Throw(new r5js.ImmutableError(''))); // Example from R5RS 6.3.5
//  expect("(vector-set! '#(0 1 2) 1 \"doe\")").
//      to(Throw(new r5js.ImmutableError(''))); // Example from R5RS 6.3.6
//  expect('(make-vector)').
//      to(Throw(new r5js.TooFewVarargs('make-vector', 1, 0)));
//  expect('(make-vector 1 2 3 4 5)').
//      to(Throw(new r5js.TooManyVarargs('make-vector', 2, 5)));
//  expect('(let ((foo (lambda (x . y) x))) (foo))').
//      to(Throw(new r5js.TooFewVarargs('', 1, 0)));
//  expect('(+ "a" "b")').
//      to(Throw(new r5js.ArgumentTypeError(
//          'a', 0, '+', r5js.DatumType.NUMBER, r5js.DatumType.STRING)));
//  expect('(scheme-report-environment 6)').
//      to(Throw(new r5js.UnimplementedOptionError('')));
//  expect('(null-environment 6)').
//      to(Throw(new r5js.UnimplementedOptionError('')));
//};

