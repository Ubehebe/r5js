import {Datum} from "../ast/datum";
import {
  ASSIGNMENT,
  CONDITIONAL,
  DEFINITION,
  FORMALS,
  LAMBDA_EXPRESSION,
  MACRO_BLOCK,
  Nonterminal,
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
  VARIABLE
} from "../parse/nonterminals";
import {ParserImpl} from "../parse/parser_impl";
import {newReader} from "../read/reader";
import {newTokenStream} from "../scan/token_stream";

describe("parser", () => {

  beforeEach(() => {
    jasmine.addMatchers({toParseAs});
  });

  describe("variables", () => {
    it("should accept well-formed variables", () => {
      [
        '...',
        '+',
        '-',
        'x'
      ].forEach(text => expect(text).toParseAs(VARIABLE));
    });
    it("should reject malformed variables", () => {
      expect('(').not.toParseAs(VARIABLE);
    });
  });

  describe("quotations", () => {
    it("should accept well-formed quotations", () => {
      [
        "'1",
        "''1",
        '(quote quote)',
        "'quote"
      ].forEach(text => expect(text).toParseAs(QUOTATION));
    });

    it("should reject malformed quotations", () => {
      [
        'quote',
        "''"
      ].forEach(text => expect(text).not.toParseAs(QUOTATION));
    });
  });

  describe("self-evaluating", () => {
    it("should accept well-formed self-evaluating forms", () => {
      [
        '#t',
        '1',
        '#\\a',
        '#\\space',
        '3.14159',
        '"hello, world"',
        '"(define foo x y)"'
      ].forEach(text => expect(text).toParseAs(SELF_EVALUATING));
    });

    it("should reject forms that are not self-evaluating", () => {
      [
        '(define foo (+ 1 2))',
        '+'
      ].forEach(text => expect(text).not.toParseAs(SELF_EVALUATING));
    });
  });

  describe("procedure calls", () => {
    it("should accept well-formed procedure calls", () => {
      [
        '(+)',
        '(define x)',
        '((foo) (foo))',
        '((define) foo)',
        '((lambda () +) 1 2)'
      ].forEach(text => expect(text).toParseAs(PROCEDURE_CALL));
    });

    it("should reject malformed procedure calls", () => {
      [
        '(foo x',
        'foo x)',
        '()',
        '(foo x y . z)'
        // TODO bl parses as a macro use '((define) define)'
      ].forEach(text => expect(text).not.toParseAs(PROCEDURE_CALL));
    });
  });

  describe("lambda expressions", () => {
    it("should accept well-formed lambda expressions", () => {
      [
        '(lambda () 1)',
        '(lambda x 1)',
        '(lambda (x) y z)',
        '(lambda (x y) (x y))',
        '(lambda (x . y) z)',
        '(lambda () (define x 1) (define y 2) x)',
        '(lambda () (define x 1) (define y 2) x y)'
      ].forEach(text => expect(text).toParseAs(LAMBDA_EXPRESSION));
    });

    it("should reject malformed lambda expressions", () => {
      [
        '(lambda (x y))',
        '(lambda x . y z)',
        '(lambda lambda)',
        '(lambda () (define x 1) (define y 2))'
      ].forEach(text => expect(text).not.toParseAs(LAMBDA_EXPRESSION));
    });
  });

  describe("formals", () => {
    it("should accept well-formed formals", () => {
      [
        '(x y z)',
        'x',
        '(x . z)'
      ].forEach(text => expect(text).toParseAs(FORMALS));
    });

    it("should reject malformed formals", () => {
      [
        '( . x)',
        '(x . y . z)'
      ].forEach(text => expect(text).not.toParseAs(FORMALS));
    });
  });

  describe("definitions", () => {
    it("should accept well-formed definitions", () => {
      [
        '(define x x)',
        '(define (foo x y) (foo x y))',
        '(begin (define x x) (define y y))',
        '(define (x . y) 1)',
        '(begin)',
        '(define (x) (define y 1) x)',
        '(begin (define x 1) (define y 2))'
      ].forEach(text => expect(text).toParseAs(DEFINITION));
    });

    it("should reject malformed definitions", () => {
      [
        'define',
        '(define x)',
        '(begin 1)',
        '(begin ())'
      ].forEach(text => expect(text).not.toParseAs(DEFINITION));
    });
  });

  describe("conditionals", () => {
    it("should accept well-formed conditionals", () => {
      [
        '(if x y z)',
        '(if x y)',
        '(if x (define x 1))'
      ].forEach(text => expect(text).toParseAs(CONDITIONAL));
    });

    it("should reject malformed conditionals", () => {
      [
        '(if x)',
        '(if)'
      ].forEach(text => expect(text).not.toParseAs(CONDITIONAL));
    });
  });

  describe("assignments", () => {
    it("should accept well-formed assignments", () => {
      [
        '(set! let! met!)'
      ].forEach(text => expect(text).toParseAs(ASSIGNMENT));
    });

    it("should reject malformed assignments", () => {
      [
        '(set!)',
        '(set! set!)',
        '(set! x)'
      ].forEach(text => expect(text).not.toParseAs(ASSIGNMENT));
    });
  });

  describe("transformer spec", () => {
    it("should accept well-formed transformer specs", () => {
      expect('(syntax-rules ())').toParseAs(TRANSFORMER_SPEC);
    });
    it("should reject malformed transformer specs", () => {
      expect('(syntax-rules)').not.toParseAs(TRANSFORMER_SPEC);
    });
  });

  describe("pattern identifier", () => {
    it("should accept well-formed pattern identifiers", () => {
      expect('define').toParseAs(PATTERN_IDENTIFIER);
      expect('x').toParseAs(PATTERN_IDENTIFIER);
    });

    it("should reject malformed pattern identifiers", () => {
      expect('...').not.toParseAs(PATTERN_IDENTIFIER);
    });
  });

  describe("patterns", () => {
    it("should accept well-formed patterns", () => {
      [
        '()',
        '(define)',
        '(define ...)',
        '(define . define)',
        '#()',
        '#(define ...)'
      ].forEach(text => expect(text).toParseAs(PATTERN));
    });

    it("should reject malformed patterns", () => {
      [
        '(define . ...)',
        '(...)'
      ].forEach(text => expect(text).not.toParseAs(PATTERN));
    });
  });

  describe("pattern datums", () => {
    it("should accept well-formed pattern datums", () => {
      expect('"x"').toParseAs(PATTERN_DATUM);
    });
    it("should reject malformed pattern datums", () => {
      expect('x').not.toParseAs(PATTERN_DATUM);
      expect("'x").not.toParseAs(PATTERN_DATUM);
    });
  });

  describe("templates", () => {
    it("should accept well-formed templates", () => {
      [
        '()',
        '#()',
        '(x...)',
        '(x... . x)',
        '(x... y...)'
      ].forEach(text => expect(text).toParseAs(TEMPLATE));
    });
  });

  describe("quasiquotation", () => {
    it("should accept well-formed quasiquotations", () => {
      [
        '`(list ,(+ 1 2) 4)',
        "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)",
        "`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))",
        "`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)",
        '`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)'
      ].forEach(text => expect(text).toParseAs(QUASIQUOTATION));
    });

    it("should reject malformed quasiquotations", () => {
      expect("(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)")
          .not
          .toParseAs(QUASIQUOTATION);
    });
  });

  describe("splicing unquotation", () => {
    it("should accept well-formed splicing unquotations", () => {
      [
        ",@(cdr '(c))",
        "(unquote-splicing (cdr '(c)))"
      ].forEach(text => expect(text).toParseAs(SPLICING_UNQUOTATION));
    });

    it("should reject malformed splicing unquotations", () => {
      [
        ',@',
        'unquote-splicing'
      ].forEach(text => expect(text).not.toParseAs(SPLICING_UNQUOTATION));
    });
  });

  describe("macro blocks", () => {
    it("should accept well-formed macro blocks", () => {
      [
        '(let-syntax () 1)',
        '(letrec-syntax () 1)',
        "(let-syntax ((foo (syntax-rules () ((foo x) 'x)))) 1)",
        "(letrec-syntax ((foo (syntax-rules (x) ((foo x) 'x)))) (foo))",
        '(let-syntax ((foo (syntax-rules () ((foo) (+ 1 2 3))))) (define x 12) x)'
      ].forEach(text => expect(text).toParseAs(MACRO_BLOCK));
    });

    it("should reject malformed macro blocks", () => {
      [
        '(let-syntax ())',
        '(letrec-syntax ())'
      ].forEach(text => expect(text).not.toParseAs(MACRO_BLOCK));
    });
  });

  describe("programs", () => {
    it("should accept well-formed programs", () => {
      [
        '',
        ' ',
        '  ',
        '    ',
        '\n',
        '\t',
        '\n \t    \n\n \t \n'
      ].forEach(text => expect(text).toParseAs(PROGRAM));
    });

    it("should reject malformed programs", () => {
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
      ].forEach(text => expect(text).not.toParseAs(PROGRAM));
    });
  });
});

function toParseAs(util: jasmine.MatchersUtil, customEqualityTesters: jasmine.CustomEqualityTester[]): jasmine.CustomMatcher {
  return {
    compare(actual: any, expected: Nonterminal): jasmine.CustomMatcherResult {
      let datumRoot;
      try {
        datumRoot = newReader(newTokenStream(actual)).read();
      } catch (e) {
        return {
          pass: false,
          message: e.message,
        };
      }
      const actualResult = (datumRoot instanceof Datum)
          && new ParserImpl(datumRoot).parse(expected);
      let actualType: Nonterminal | null = null;
      if (actualResult && actualResult.peekParse) {
        actualType = actualResult.peekParse();
      }

      return {
        pass: actualType === expected,
        message: `expected ${actual} to parse as ${expected}, got ${actualType}`,
      };
    }
  };
}
