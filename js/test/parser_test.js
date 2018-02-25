goog.module('r5js.test.Parser');
goog.setTestOnly('r5js.test.Parser');

const expect = goog.require('expect');
const parseAs = goog.require('parseAs');
const testSuite = goog.require('goog.testing.testSuite');
const {
  ASSIGNMENT,
  CONDITIONAL,
  DEFINITION,
  FORMALS,
  LAMBDA_EXPRESSION,
  MACRO_BLOCK,
  PATTERN,
  PATTERN_DATUM,
  PATTERN_IDENTIFIER,
  PROCEDURE_CALL,
  PROGRAM,
  QUASIQUOTATION,
  QUOTATION,
  SELF_EVALUATING,
  SPLICING_UNQUOTATION,
  TEMPLATE,
  TRANSFORMER_SPEC,
  VARIABLE,
} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');
goog.require('goog.testing.jsunit');

testSuite({

  testVariable() {
    [
      '...',
      '+',
      '-',
      'x'
    ].forEach(text => expect(text).to(parseAs(VARIABLE)));

    [
      '('
    ].forEach(text => expect(text).not().to(parseAs(VARIABLE)));
  },

  testQuotation() {
    [
      "'1",
      "''1",
      '(quote quote)',
      "'quote"
    ].forEach(text => expect(text).to(parseAs(QUOTATION)));

    [
      'quote',
      "''"
    ].forEach(text => expect(text).not().to(parseAs(QUOTATION)));
  },

  testSelfEvaluating() {
    [
      '#t',
      '1',
      '#\\a',
      '#\\space',
      '3.14159',
      '"hello, world"',
      '"(define foo x y)"'
    ].forEach(text => expect(text).to(parseAs(SELF_EVALUATING)));
    [
      '(define foo (+ 1 2))',
      '+'
    ].forEach(text => expect(text).not().to(parseAs(SELF_EVALUATING)));
  },

  testProcedureCall() {
    [
      '(+)',
      '(define x)',
      '((foo) (foo))',
      '((define) foo)',
      '((lambda () +) 1 2)'
    ].forEach(text => expect(text).to(parseAs(PROCEDURE_CALL)));

    [
      '(foo x',
      'foo x)',
      '()',
      '(foo x y . z)'
      // TODO bl parses as a macro use '((define) define)'
    ].forEach(text => expect(text).not().to(parseAs(PROCEDURE_CALL)));
  },

  testLambdaExpression() {
    [
      '(lambda () 1)',
      '(lambda x 1)',
      '(lambda (x) y z)',
      '(lambda (x y) (x y))',
      '(lambda (x . y) z)',
      '(lambda () (define x 1) (define y 2) x)',
      '(lambda () (define x 1) (define y 2) x y)'
    ].forEach(text => expect(text).to(parseAs(LAMBDA_EXPRESSION)));

    [
      '(lambda (x y))',
      '(lambda x . y z)',
      '(lambda lambda)',
      '(lambda () (define x 1) (define y 2))'
    ].forEach(text => expect(text).not().to(parseAs(LAMBDA_EXPRESSION)));
  },

  testFormals() {
    [
      '(x y z)',
      'x',
      '(x . z)'
    ].forEach(text => expect(text).to(parseAs(FORMALS)));

    [
      '( . x)',
      '(x . y . z)'
    ].forEach(text => expect(text).not().to(parseAs(FORMALS)));
  },

  testDefinition() {
    [
      '(define x x)',
      '(define (foo x y) (foo x y))',
      '(begin (define x x) (define y y))',
      '(define (x . y) 1)',
      '(begin)',
      '(define (x) (define y 1) x)',
      '(begin (define x 1) (define y 2))'
    ].forEach(text => expect(text).to(parseAs(DEFINITION)));

    [
      'define',
      '(define x)',
      '(begin 1)',
      '(begin ())'
    ].forEach(text => expect(text).not().to(parseAs(DEFINITION)));
  },

  testConditional() {
    [
      '(if x y z)',
      '(if x y)',
      '(if x (define x 1))'
    ].forEach(text => expect(text).to(parseAs(CONDITIONAL)));

    [
      '(if x)',
      '(if)'
    ].forEach(text => expect(text).not().to(parseAs(CONDITIONAL)));
  },

  testAssignment() {
    [
      '(set! let! met!)'
    ].forEach(text => expect(text).to(parseAs(ASSIGNMENT)));

    [
      '(set!)',
      '(set! set!)',
      '(set! x)'
    ].forEach(text => expect(text).not().to(parseAs(ASSIGNMENT)));
  },

  testTransformerSpec() {
    expect('(syntax-rules ())').to(parseAs(TRANSFORMER_SPEC));
    expect('(syntax-rules)').not().to(parseAs(TRANSFORMER_SPEC));
  },

  testPatternIdentifier() {
    expect('define').to(parseAs(PATTERN_IDENTIFIER));
    expect('...').not().to(parseAs(PATTERN_IDENTIFIER));
    expect('x').to(parseAs(PATTERN_IDENTIFIER));
  },

  testPattern() {
    [
      '()',
      '(define)',
      '(define ...)',
      '(define . define)',
      '#()',
      '#(define ...)'
    ].forEach(text => expect(text).to(parseAs(PATTERN)));
    [
      '(define . ...)',
      '(...)'
    ].forEach(text => expect(text).not().to(parseAs(PATTERN)));
  },

  testPatternDatum() {
    expect('x').not().to(parseAs(PATTERN_DATUM));
    expect('"x"').to(parseAs(PATTERN_DATUM));
    expect("'x").not().to(parseAs(PATTERN_DATUM));
  },

  testTemplate() {
    [
      '()',
      '#()',
      '(x...)',
      '(x... . x)',
      '(x... y...)'
    ].forEach(text => expect(text).to(parseAs(TEMPLATE)));
  },

  testQuasiquotation() {
    [
      '`(list ,(+ 1 2) 4)',
      "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)",
      "`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))",
      "`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)",
      '`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)'
    ].forEach(text => expect(text).to(parseAs(QUASIQUOTATION)));

    expect("(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)")
      .not()
      .to(parseAs(QUASIQUOTATION));
  },

  testSplicingUnquotation() {
    [
      ",@(cdr '(c))",
      "(unquote-splicing (cdr '(c)))"
    ].forEach(text => expect(text).to(parseAs(SPLICING_UNQUOTATION)));

    [
      ',@',
      'unquote-splicing'
    ].forEach(text => expect(text).not().to(parseAs(SPLICING_UNQUOTATION)));
  },

  testMacroBlock() {
    [
      '(let-syntax () 1)',
      '(letrec-syntax () 1)',
      "(let-syntax ((foo (syntax-rules () ((foo x) 'x)))) 1)",
      "(letrec-syntax ((foo (syntax-rules (x) ((foo x) 'x)))) (foo))",
      '(let-syntax ((foo (syntax-rules () ((foo) (+ 1 2 3))))) (define x 12) x)'
    ].forEach(text => expect(text).to(parseAs(MACRO_BLOCK)));

    [
      '(let-syntax ())',
      '(letrec-syntax ())'
    ].forEach(text => expect(text).not().to(parseAs(MACRO_BLOCK)));
  },

  testProgram() {
    [
      '',
      ' ',
      '  ',
      '    ',
      '\n',
      '\t',
      '\n \t    \n\n \t \n'
    ].forEach(text => expect(text).to(parseAs(PROGRAM)));
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
    ].forEach(text => expect(text).not().to(parseAs(PROGRAM)));
  }
});
