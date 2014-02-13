goog.provide('r5js.test.Parser');
goog.setTestOnly('r5js.test.Parser');


goog.require('expect');
goog.require('parseAs');
goog.require('r5js.parse.Nonterminals');
goog.require('tdd.TestType');



/**
 * @implements {tdd.TestSuite}
 * @struct
 * @constructor
 */
r5js.test.Parser = function() {};


/** @override */
r5js.test.Parser.prototype.getType = function() {
  return tdd.TestType.UNIT;
};


/** @override */
r5js.test.Parser.prototype.toString = function() {
  return 'r5js.test.Parser';
};


r5js.test.Parser.prototype['testVariable'] = function() {
  [
   '...',
   '+',
   '-',
   'x'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.VARIABLE));
  });

  [
   '('
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminals.VARIABLE));
  });
};


r5js.test.Parser.prototype['testQuotation'] = function() {
  [
   "'1",
   "''1",
   '(quote quote)',
   "'quote"
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.QUOTATION));
  });

  [
   'quote',
   "''"
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminals.QUOTATION));
  });
};


r5js.test.Parser.prototype['testSelfEvaluating'] = function() {
  [
   '#t',
   '1',
   '#\\a',
   '#\\space',
   '3.14159',
   '"hello, world"',
   '"(define foo x y)"'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.SELF_EVALUATING));
  });
  [
   '(define foo (+ 1 2))',
   '+'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminals.SELF_EVALUATING));
  });
};


r5js.test.Parser.prototype['testProcedureCall'] = function() {
  [
   '(+)',
   '(define x)',
   '((foo) (foo))',
   '((define) foo)',
   '((lambda () +) 1 2)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.PROCEDURE_CALL));
  });

  [
   '(foo x',
   'foo x)',
   '()',
   '(foo x y . z)'
   // TODO bl parses as a macro use '((define) define)'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminals.PROCEDURE_CALL));
  });
};


r5js.test.Parser.prototype['testLambdaExpression'] = function() {
  [
   '(lambda () 1)',
   '(lambda x 1)',
   '(lambda (x) y z)',
   '(lambda (x y) (x y))',
   '(lambda (x . y) z)',
   '(lambda () (define x 1) (define y 2) x)',
   '(lambda () (define x 1) (define y 2) x y)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.LAMBDA_EXPRESSION));
  });

  [
   '(lambda (x y))',
   '(lambda x . y z)',
   '(lambda lambda)',
   '(lambda () (define x 1) (define y 2))'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminals.LAMBDA_EXPRESSION));
  });
};

r5js.test.Parser.prototype['testFormals'] = function() {
  [
   '(x y z)',
   'x',
   '(x . z)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.FORMALS));
  });

  [
   '( . x)',
   '(x . y . z)'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminals.FORMALS));
  });
};


r5js.test.Parser.prototype['testDefinition'] = function() {
  [
   '(define x x)',
   '(define (foo x y) (foo x y))',
   '(begin (define x x) (define y y))',
   '(define (x . y) 1)',
   '(begin)',
   '(define (x) (define y 1) x)',
   '(begin (define x 1) (define y 2))'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.DEFINITION));
  });

  [
   'define',
   '(define x)',
   '(begin 1)',
   '(begin ())'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminals.DEFINITION));
  });
};

r5js.test.Parser.prototype['testConditional'] = function() {
  [
   '(if x y z)',
   '(if x y)',
   '(if x (define x 1))'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.CONDITIONAL));
  });

  [
   '(if x)',
   '(if)'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminals.CONDITIONAL));
  });
};


r5js.test.Parser.prototype['testAssignment'] = function() {
  [
   '(set! let! met!)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.ASSIGNMENT));
  });

  [
   '(set!)',
   '(set! set!)',
   '(set! x)'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminals.ASSIGNMENT));
  });
};


r5js.test.Parser.prototype['testTransformerSpec'] = function() {
  expect('(syntax-rules ())').to(parseAs('transformer-spec'));
  expect('(syntax-rules)').not().to(parseAs(
      r5js.parse.Nonterminals.TRANSFORMER_SPEC));
};


r5js.test.Parser.prototype['testPatternIdentifier'] = function() {
  expect('define').to(parseAs(r5js.parse.Nonterminals.PATTERN_IDENTIFIER));
  expect('...').not().to(parseAs(r5js.parse.Nonterminals.PATTERN_IDENTIFIER));
  expect('x').to(parseAs(r5js.parse.Nonterminals.PATTERN_IDENTIFIER));
};


r5js.test.Parser.prototype['testPattern'] = function() {
  [
   '()',
   '(define)',
   '(define ...)',
   '(define . define)',
   '#()',
   '#(define ...)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.PATTERN));
  });
  [
   '(define . ...)',
   '(...)'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminals.PATTERN));
  });
};


r5js.test.Parser.prototype['testPatternDatum'] = function() {
  expect('x').not().to(parseAs(r5js.parse.Nonterminals.PATTERN_DATUM));
  expect('"x"').to(parseAs(r5js.parse.Nonterminals.PATTERN_DATUM));
  expect("'x").not().to(parseAs(r5js.parse.Nonterminals.PATTERN_DATUM));
};


r5js.test.Parser.prototype['testTemplate'] = function() {
  [
   '()',
   '#()',
   '(x...)',
   '(x... . x)',
   '(x... y...)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.TEMPLATE));
  });
};


r5js.test.Parser.prototype['testQuasiquotation'] = function() {
  [
   '`(list ,(+ 1 2) 4)',
   "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)",
   "`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))",
   "`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)",
   '`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.QUASIQUOTATION));
  });

  expect("(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)")
        .not()
        .to(parseAs(r5js.parse.Nonterminals.QUASIQUOTATION));
};


r5js.test.Parser.prototype['testSplicingUnquotation'] = function() {
  [
   ",@(cdr '(c))",
   "(unquote-splicing (cdr '(c)))"
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.SPLICING_UNQUOTATION));
  });

  [
   ',@',
   'unquote-splicing'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(
        r5js.parse.Nonterminals.SPLICING_UNQUOTATION));
  });
};


r5js.test.Parser.prototype['testMacroBlock'] = function() {
  [
   '(let-syntax () 1)',
   '(letrec-syntax () 1)',
   "(let-syntax ((foo (syntax-rules () ((foo x) 'x)))) 1)",
   "(letrec-syntax ((foo (syntax-rules (x) ((foo x) 'x)))) (foo))",
   '(let-syntax ((foo (syntax-rules () ((foo) (+ 1 2 3))))) (define x 12) x)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminals.MACRO_BLOCK));
  });

  [
   '(let-syntax ())',
   '(letrec-syntax ())'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminals.MACRO_BLOCK));
  });
};

