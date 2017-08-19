goog.provide('r5js.test.Parser');
goog.setTestOnly('r5js.test.Parser');

goog.require('expect');
goog.require('goog.testing.jsunit');
goog.require('parseAs');
goog.require('r5js.parse.Nonterminal');

function testVariable() {
  [
   '...',
   '+',
   '-',
   'x'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.VARIABLE));
  });

  [
   '('
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.VARIABLE));
  });
}

function testQuotation() {
  [
   "'1",
   "''1",
   '(quote quote)',
   "'quote"
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.QUOTATION));
  });

  [
   'quote',
   "''"
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.QUOTATION));
  });
}

function testSelfEvaluating() {
  [
   '#t',
   '1',
   '#\\a',
   '#\\space',
   '3.14159',
   '"hello, world"',
   '"(define foo x y)"'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.SELF_EVALUATING));
  });
  [
   '(define foo (+ 1 2))',
   '+'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.SELF_EVALUATING));
  });
}

function testProcedureCall() {
  [
   '(+)',
   '(define x)',
   '((foo) (foo))',
   '((define) foo)',
   '((lambda () +) 1 2)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.PROCEDURE_CALL));
  });

  [
   '(foo x',
   'foo x)',
   '()',
   '(foo x y . z)'
   // TODO bl parses as a macro use '((define) define)'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.PROCEDURE_CALL));
  });
}

function testLambdaExpression() {
  [
   '(lambda () 1)',
   '(lambda x 1)',
   '(lambda (x) y z)',
   '(lambda (x y) (x y))',
   '(lambda (x . y) z)',
   '(lambda () (define x 1) (define y 2) x)',
   '(lambda () (define x 1) (define y 2) x y)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.LAMBDA_EXPRESSION));
  });

  [
   '(lambda (x y))',
   '(lambda x . y z)',
   '(lambda lambda)',
   '(lambda () (define x 1) (define y 2))'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.LAMBDA_EXPRESSION));
  });
}

function testFormals() {
  [
   '(x y z)',
   'x',
   '(x . z)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.FORMALS));
  });

  [
   '( . x)',
   '(x . y . z)'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.FORMALS));
  });
}

function testDefinition() {
  [
   '(define x x)',
   '(define (foo x y) (foo x y))',
   '(begin (define x x) (define y y))',
   '(define (x . y) 1)',
   '(begin)',
   '(define (x) (define y 1) x)',
   '(begin (define x 1) (define y 2))'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.DEFINITION));
  });

  [
   'define',
   '(define x)',
   '(begin 1)',
   '(begin ())'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.DEFINITION));
  });
}

function testConditional() {
  [
   '(if x y z)',
   '(if x y)',
   '(if x (define x 1))'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.CONDITIONAL));
  });

  [
   '(if x)',
   '(if)'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.CONDITIONAL));
  });
}

function testAssignment() {
  [
   '(set! let! met!)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.ASSIGNMENT));
  });

  [
   '(set!)',
   '(set! set!)',
   '(set! x)'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.ASSIGNMENT));
  });
}

function testTransformerSpec() {
  expect('(syntax-rules ())').to(parseAs(
      r5js.parse.Nonterminal.Nonterminals.TRANSFORMER_SPEC));
  expect('(syntax-rules)').not().to(parseAs(
      r5js.parse.Nonterminal.Nonterminals.TRANSFORMER_SPEC));
}

function testPatternIdentifier() {
  expect('define').to(parseAs(r5js.parse.Nonterminal.Nonterminals.PATTERN_IDENTIFIER));
  expect('...').not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.PATTERN_IDENTIFIER));
  expect('x').to(parseAs(r5js.parse.Nonterminal.Nonterminals.PATTERN_IDENTIFIER));
}

function testPattern() {
  [
   '()',
   '(define)',
   '(define ...)',
   '(define . define)',
   '#()',
   '#(define ...)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.PATTERN));
  });
  [
   '(define . ...)',
   '(...)'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.PATTERN));
  });
}

function testPatternDatum() {
  expect('x').not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.PATTERN_DATUM));
  expect('"x"').to(parseAs(r5js.parse.Nonterminal.Nonterminals.PATTERN_DATUM));
  expect("'x").not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.PATTERN_DATUM));
}

function testTemplate() {
  [
   '()',
   '#()',
   '(x...)',
   '(x... . x)',
   '(x... y...)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.TEMPLATE));
  });
}

function testQuasiquotation() {
  [
   '`(list ,(+ 1 2) 4)',
   "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)",
   "`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))",
   "`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)",
   '`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.QUASIQUOTATION));
  });

  expect("(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)")
        .not()
        .to(parseAs(r5js.parse.Nonterminal.Nonterminals.QUASIQUOTATION));
}

function testSplicingUnquotation() {
  [
   ",@(cdr '(c))",
   "(unquote-splicing (cdr '(c)))"
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.SPLICING_UNQUOTATION));
  });

  [
   ',@',
   'unquote-splicing'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(
        r5js.parse.Nonterminal.Nonterminals.SPLICING_UNQUOTATION));
  });
}

function testMacroBlock() {
  [
   '(let-syntax () 1)',
   '(letrec-syntax () 1)',
   "(let-syntax ((foo (syntax-rules () ((foo x) 'x)))) 1)",
   "(letrec-syntax ((foo (syntax-rules (x) ((foo x) 'x)))) (foo))",
   '(let-syntax ((foo (syntax-rules () ((foo) (+ 1 2 3))))) (define x 12) x)'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.MACRO_BLOCK));
  });

  [
   '(let-syntax ())',
   '(letrec-syntax ())'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.MACRO_BLOCK));
  });
}

function testProgram() {
  [
   '',
   ' ',
   '  ',
   '    ',
   '\n',
   '\t',
   '\n \t    \n\n \t \n'
  ].forEach(function(text) {
    expect(text).to(parseAs(r5js.parse.Nonterminal.Nonterminals.PROGRAM));
  });
  [
   '(',
   ')',
   '((',
   '()',
   ')(',
   '))',
   '(((',
   '(()',
   '()(',
   '())',
   ')((',
   ')()',
   '))(',
   ')))',
   '((((',
   '((()',
   '(()(',
   '(())',
   '()((',
   '()()',
   '())(',
   '()))',
   ')(((',
   ')(()',
   ')()(',
   ')())',
   '))((',
   '))()',
   ')))(',
   '))))'
  ].forEach(function(text) {
    expect(text).not().to(parseAs(r5js.parse.Nonterminal.Nonterminals.PROGRAM));
  });
}
