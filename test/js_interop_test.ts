import {NUMBER, STRING} from "../ast/types";
import {Error, ErrorType} from "../base/error";
import {boot} from "../eval/boot";
import {Evaluator} from "../eval/evaluator";
import {InMemoryInputPort} from "../io/in_memory_input_port";
import {InMemoryOutputPort} from "../io/in_memory_output_port";
import {InMemoryPortBuffer} from "../io/in_memory_port_buffer";
import {LPAREN, RPAREN} from "../parse/terminals";
import {argumentTypeError} from "../runtime/errors";
import {SchemeSources} from "../scm/scheme_sources";

let buffer;
let stdin;
let stdout;
let sources;
let evaluator: Evaluator;
let sharedOutputPort: InMemoryOutputPort | undefined;

describe("scheme<->js interop tests", () => {
  beforeEach(() => {
    jasmine.addMatchers({toEvalTo, toOutput, toThrow});
    // TODO: the output buffer isn't correctly flushed. Promoting these to top-level consts
    // causes some tests to fail with garbage from previous tests.
    buffer = new InMemoryPortBuffer();
    stdin = new InMemoryInputPort(buffer);
    stdout = new InMemoryOutputPort(buffer);
    sharedOutputPort = stdout;
    sources = new SchemeSources();
    evaluator = boot(sources.syntax, sources.procedures, stdin, stdout);
  });

  it("should return primitives to js correctly", () => {
    expect('42').toEvalTo('42');
    expect('42').toEvalTo('42');
    expect('#t').toEvalTo('#t');
    expect('#f').toEvalTo('#f');
    expect('"hello, world"').toEvalTo('"hello, world"');
    expect("'hello").toEvalTo('hello');
    expect('(quote hello)').toEvalTo('hello');
    expect('#\\a').toEvalTo('#\\a');
    expect('#\\space').toEvalTo('#\\space');
    expect('#\\newline').toEvalTo('#\\newline');
  });

  it("should display primitives to js correctly", () => {
    expect('(display 42)').toOutput('42');
    expect('(display #t)').toOutput('#t');
    expect('(display #f)').toOutput('#f');
    expect('(display "hello, world")').toOutput('hello, world');
    expect("(display 'hello)").toOutput('hello');
    expect('(display (quote hello))').toOutput('hello');
    expect('(display #\\a)').toOutput('a');
    expect('(display #\\space)').toOutput(' ');
    expect('(display #\\newline)').toOutput('\n');
  });

  it("should write primitives correctly to js", () => {
    expect('(write 42)').toOutput('42');
    expect('(write #t)').toOutput('#t');
    expect('(write #f)').toOutput('#f');
    expect('(write "hello, world")').toOutput('"hello, world"');
    expect("(write 'hello)").toOutput('hello');
    expect('(write (quote hello))').toOutput('hello');
    expect('(write #\\a)').toOutput('#\\a');
    expect('(write #\\space)').toOutput('#\\space');
    expect('(write #\\newline)').toOutput('#\\newline');
  });

  it("should execute sanity checks correctly", () => {
    expect('(+ 1 1)').toEvalTo('2');
    expect('(procedure? procedure?)').toEvalTo('#t');
    expect('(string-append "hello " "world")').toEvalTo('"hello world"');
    expect("'a").toEvalTo('a');
    expect("''a").toEvalTo("'a");
    expect("'''a").toEvalTo("''a");
    expect("''''a").toEvalTo("'''a");
    expect("'''''a").toEvalTo("''''a");
  });

  it("should return recursive types to js correctly", () => {
    expect('#()').toEvalTo('#()');
    expect("'()").toEvalTo('()');
    expect("(list '() '() '() '(42))").toEvalTo('(() () () (42))');
    expect('(list 1 2 3)').toEvalTo('(1 2 3)');
    expect("(cons 'a (cons 'b (cons 'c '())))").toEvalTo('(a b c)');
    expect("(cons 'a 'b)").toEvalTo('(a . b)');
  });

  it("should display recursive types to js correctly", () => {
    expect('(display #())').toOutput('#()');
    expect("(display '())").toOutput('()');
    expect("(display (list '() '() '() '(42)))").toOutput('(() () () (42))');
    expect('(display (list 1 2 3))').toOutput('(1 2 3)');
    expect("(display (cons 'a (cons 'b (cons 'c '()))))").toOutput('(a b c)');
    expect("(display (cons 'a 'b))").toOutput('(a . b)');
  });

  it("should write recursive types to js correctly", () => {
    expect('(write #())').toOutput('#()');
    expect("(write '())").toOutput('()');
    expect("(write (list '() '() '() '(42)))").toOutput('(() () () (42))');
    expect('(write (list 1 2 3))').toOutput('(1 2 3)');
    expect("(write (cons 'a (cons 'b (cons 'c '()))))").toOutput('(a b c)');
    expect("(write (cons 'a 'b))").toOutput('(a . b)');
  });

  // R5RS doesn't actually forbid these external representations to be the empty string, but empty
  // strings are not helpful to return in a REPL.
  it("should do something reasonable with nonstandard external representations", () => {
    expect('+').not.toEvalTo('');
    expect('(lambda (x) x)').not.toEvalTo('');
    expect('(current-input-port)').not.toEvalTo('');
    expect('(current-output-port)').not.toEvalTo('');
    expect('(scheme-report-environment 5)').not.toEvalTo('');
    expect('(null-environment 5)').not.toEvalTo('');
  });

  it("should return the empty string for unspecified values", () => {
    expect('').toEvalTo('');
    expect(' ').toEvalTo('');
    expect('\n').toEvalTo('');
    expect('\t').toEvalTo('');
    expect('    \t \n\n\n   ').toEvalTo('');
    expect('(define x 1)').toEvalTo('');
    expect('(define x 1) (set! x 2)').toEvalTo('');
    expect('(define x (cons 1 2)) (set-car! x x)').toEvalTo('');
    expect('(define x (cons 1 2)) (set-cdr! x x)').toEvalTo('');
    expect('(if #f #t)').toEvalTo('');
    expect('(write "foo")').toEvalTo('');
    expect('(display 42)').toEvalTo('');
    expect('(write-char #\\a)').toEvalTo('');
    expect('(close-input-port (current-input-port))').toEvalTo('');
    expect('(close-input-port (open-input-file "foo"))').toEvalTo('');
    expect('(close-output-port (open-output-file "foo"))').toEvalTo('');
    expect('(close-output-port (current-output-port))').toEvalTo('');
  });

  // TODO all these errors stringify to [Object object], so the tests are not very valuable. Fix.
  it("should throw appropriate errors", () => {
    expect('(').toThrow(new Error(ErrorType.READ, `read error: ${LPAREN}`));
    expect(')').toThrow(new Error(ErrorType.READ, `read error: ${RPAREN}`));
    expect('(eval)').toThrow(Error.incorrectNumArgs('eval', 2, 0));
    expect('(eval 1 2 3 4 5)').toThrow(Error.incorrectNumArgs('eval', 2, 5));
    expect('(let ((foo (lambda (x) x))) (foo))').toThrow(Error.incorrectNumArgs(''/* TODO bl lambda */, 1, 0));
    expect('(let ((foo (lambda (x) x))) (foo 1 2))').toThrow(Error.incorrectNumArgs('' /* TODO bl lambda */, 1, 2));
    expect("(set-car! '(1 2 3) 4)").toThrow(Error.immutable(''));
    // Example from R5RS 6.3.5
    expect('(let ((g (lambda () "***"))) (string-set! (g) 0 #\\?))').toThrow(Error.immutable(''));
    // Example from R5RS 6.3.5
    expect("(string-set! (symbol->string 'immutable) 0 #\\?)").toThrow(Error.immutable(''));
    // Example from R5RS 6.3.6
    expect("(vector-set! '#(0 1 2) 1 \"doe\")").toThrow(Error.immutable(''));
    expect('(make-vector)').toThrow(Error.tooFewVarargs('make-vector', 1, 0));
    expect('(make-vector 1 2 3 4 5)').toThrow(Error.tooManyVarargs('make-vector', 2, 5));
    expect('(let ((foo (lambda (x . y) x))) (foo))').toThrow(Error.tooFewVarargs('', 1, 0));
    expect('(+ "a" "b")').toThrow(argumentTypeError('a', 0, '+', NUMBER, STRING));
    expect('(scheme-report-environment 6)').toThrow(Error.unimplementedOption(''));
    expect('(null-environment 6)').toThrow(Error.unimplementedOption(''));
  });
});

function toEvalTo(util: jasmine.MatchersUtil, customEqualityTesters: jasmine.CustomEqualityTester[]): jasmine.CustomMatcher {
  return {
    compare(actualInput: string, expectedResult: string): jasmine.CustomMatcherResult {
      let actualResult;
      try {
        actualResult = evaluator.evaluate(actualInput);
      } catch (e) {
        actualResult = e;
      }
      return {
        pass: actualResult === expectedResult,
        message: `want ${expectedResult}, got ${actualResult}`,
      };
    }
  };
}

function toOutput(util: jasmine.MatchersUtil, customEqualityTesters: jasmine.CustomEqualityTester[]): jasmine.CustomMatcher {
  return {
    compare(actualInput: string, expectedOutput: string): jasmine.CustomMatcherResult {
      let actualResult;
      try {
        actualResult = evaluator.evaluate(actualInput);
      } catch (e) {
        actualResult = e;
      }
      const actualOutput = sharedOutputPort!.dequeueOutput();
      return {
        pass: actualOutput === expectedOutput,
        message: `want ${expectedOutput}, got ${expectedOutput}`,
      };
    }
  };
}

function toThrow(util: jasmine.MatchersUtil, customEqualityTesters: jasmine.CustomEqualityTester[]): jasmine.CustomMatcher {
  return {
    compare(input: string, expectedError: any): jasmine.CustomMatcherResult {
      let actualError = '';
      try {
        evaluator.evaluate(input);
      } catch (e) {
        actualError = e.toString();
      }
      return {
        pass: actualError === expectedError.toString(),
        message: `want ${expectedError}, got ${actualError}`,
      };
    }
  };
}
