goog.provide('r5js.test.Parser');
goog.setTestOnly('r5js.test.Parser');


goog.require('Expect');
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
            Expect(text).toParseAs('variable');
        });

    [
        '('
    ].forEach(function(text) {
            Expect(text).not().toParseAs('variable');
        });
};


r5js.test.Parser.prototype['testQuotation'] = function() {
    [
        "'1",
        "''1",
        '(quote quote)',
        "'quote"
    ].forEach(function(text) {
            Expect(text).toParseAs('quotation');
        });

    [
        'quote',
        "''"
    ].forEach(function(text) {
            Expect(text).not().toParseAs('quotation');
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
            Expect(text).toParseAs('self-evaluating');
        });
    [
        '(define foo (+ 1 2))',
        '+'
    ].forEach(function(text) {
            Expect(text).not().toParseAs('self-evaluating');
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
            Expect(text).toParseAs('procedure-call');
        });

    [
        '(foo x',
        'foo x)',
        '()',
        '(foo x y . z)'
// TODO bl parses as a macro use '((define) define)'
    ].forEach(function(text) {
            Expect(text).not().toParseAs('procedure-call');
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
          Expect(text).toParseAs('lambda-expression');
      });

    [
        '(lambda (x y))',
        '(lambda x . y z)',
        '(lambda lambda)',
        '(lambda () (define x 1) (define y 2))'
    ].forEach(function(text) {
            Expect(text).not().toParseAs('lambda-expression');
        });
};

r5js.test.Parser.prototype['testFormals'] = function() {
  [
      '(x y z)',
      'x',
      '(x . z)'
  ].forEach(function(text) {
          Expect(text).toParseAs('formals');
      });

    [
        '( . x)',
        '(x . y . z)'
    ].forEach(function(text) {
            Expect(text).not().toParseAs('formals');
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
          Expect(text).toParseAs('definition');
      });

    [
        'define',
        '(define x)',
        '(begin 1)',
        '(begin ())'
    ].forEach(function(text) {
            Expect(text).not().toParseAs('definition');
        });
};

r5js.test.Parser.prototype['testConditional'] = function() {
    [
        '(if x y z)',
        '(if x y)',
        '(if x (define x 1))'
    ].forEach(function(text) {
            Expect(text).toParseAs('conditional');
        });

    [
        '(if x)',
        '(if)'
    ].forEach(function(text) {
            Expect(text).not().toParseAs('conditional');
        });
};


r5js.test.Parser.prototype['testAssignment'] = function() {
  [
      '(set! let! met!)'
  ].forEach(function(text) {
          Expect(text).toParseAs('assignment');
      });

    [
        '(set!)',
        '(set! set!)',
        '(set! x)'
    ].forEach(function(text) {
            Expect(text).not().toParseAs('assignment');
        });
};


r5js.test.Parser.prototype['testTransformerSpec'] = function() {
  Expect('(syntax-rules ())').toParseAs('transformer-spec');
    Expect('(syntax-rules)').not().toParseAs('transformer-spec');
};


r5js.test.Parser.prototype['testPatternIdentifier'] = function() {
  Expect('define').toParseAs('pattern-identifier');
    Expect('...').not().toParseAs('pattern-identifier');
    Expect('x').toParseAs('pattern-identifier');
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
            Expect(text).toParseAs('pattern');
        });
    [
        '(define . ...)',
        '(...)'
    ].forEach(function(text) {
            Expect(text).not().toParseAs('pattern');
        });
};


r5js.test.Parser.prototype['testPatternDatum'] = function() {
    Expect('x').not().toParseAs('pattern-datum');
    Expect('"x"').toParseAs('pattern-datum');
    Expect("'x").not().toParseAs('pattern-datum');
};


r5js.test.Parser.prototype['testTemplate'] = function() {
    [
        '()',
        '#()',
        '(x...)',
        '(x... . x)',
        '(x... y...)'
    ].forEach(function(text) {
            Expect(text).toParseAs('template');
        });
};


r5js.test.Parser.prototype['testQuasiquotation'] = function() {
  [
      '`(list ,(+ 1 2) 4)',
      "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)",
      "`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))",
      "`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)",
      "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)"
  ].forEach(function(text) {
          Expect(text).toParseAs('quasiquotation');
      });

    Expect("(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)")
        .not()
        .toParseAs('quasiquotation');
};


r5js.test.Parser.prototype['testSplicingUnquotation'] = function() {
    [
        ",@(cdr '(c))",
        "(unquote-splicing (cdr '(c)))"
    ].forEach(function(text) {
            Expect(text).toParseAs('splicing-unquotation');
        });

    [
        ',@',
        'unquote-splicing'
    ].forEach(function(text) {
            Expect(text).not().toParseAs('splicing-unquotation');
        });
};


r5js.test.Parser.prototype['testMacroBlock'] = function() {
    [
        '(let-syntax () 1)',
        '(letrec-syntax () 1)',
        "(let-syntax ((foo (syntax-rules () ((foo x) 'x)))) 1)",
        "(letrec-syntax ((foo (syntax-rules (x) ((foo x) 'x)))) (foo))",
        "(let-syntax ((foo (syntax-rules () ((foo) (+ 1 2 3))))) (define x 12) x)"
    ].forEach(function(text) {
            Expect(text).toParseAs('macro-block');
        });

    [
        '(let-syntax ())',
        '(letrec-syntax ())'
    ].forEach(function(text) {
            Expect(text).not().toParseAs('macro-block');
        });
};

