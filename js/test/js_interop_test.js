goog.module('r5js.test.JsInterop');
goog.setTestOnly('r5js.test.JsInterop');

const {InMemoryInputPort} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/in_memory_input_port');
const {InMemoryOutputPort} = require('/js/io/io_collect_es6_sources.es6/node_modules/__main__/js/io/in_memory_output_port');
const SchemeSources = goog.require('r5js.SchemeSources');
const Throw = goog.require('Throw');
const {argumentTypeError} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/errors');
const evalTo = goog.require('evalTo');
const expect = goog.require('expect');
const testSuite = goog.require('goog.testing.testSuite');
const {Error, ErrorType} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');
const {InMemoryPortBuffer} = require('/js/io/io_collect_es6_sources.es6/node_modules/__main__/js/io/in_memory_port_buffer');
const {LPAREN, RPAREN} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/parse/terminals');
const {Types} = require('/js/ast/type_collect_es6_sources.es6/node_modules/__main__/js/ast/type');
const {boot} = goog.require('r5js.boot');
const {output, setOutputPort} = goog.require('output');
goog.require('goog.testing.jsunit');

let buffer;
let stdin;
let stdout;
let sources;
let evaluator;

/** TODO */
function expect_(input) {
    let val;
    try {
        val = evaluator.evaluate(input);
    } catch (e) {
        val = e;
    }
    return expect(val);
}

testSuite({

    // TODO: the output buffer isn't correctly flushed.
    // Promoting these to top-level consts causes some tests to fail
    // with garbage from previous tests.
    setUp() {
        buffer = new InMemoryPortBuffer();
        stdin = new InMemoryInputPort(buffer);
        stdout = new InMemoryOutputPort(buffer);
        setOutputPort(stdout);
        sources = new SchemeSources();
        evaluator = boot(sources.syntax, sources.procedures, stdin, stdout);
    },

    testReturnPrimitivesToJs() {
        expect_('42').to(evalTo('42'));
        expect_('42').to(evalTo('42'));
        expect_('#t').to(evalTo('#t'));
        expect_('#f').to(evalTo('#f'));
        expect_('"hello, world"').to(evalTo('"hello, world"'));
        expect_("'hello").to(evalTo('hello'));
        expect_('(quote hello)').to(evalTo('hello'));
        expect_('#\\a').to(evalTo('#\\a'));
        expect_('#\\space').to(evalTo('#\\space'));
        expect_('#\\newline').to(evalTo('#\\newline'));
    },

    testDisplayPrimitivesToJs() {
        expect_('(display 42)').to(output('42'));
        expect_('(display #t)').to(output('#t'));
        expect_('(display #f)').to(output('#f'));
        expect_('(display "hello, world")').to(output('hello, world'));
        expect_("(display 'hello)").to(output('hello'));
        expect_('(display (quote hello))').to(output('hello'));
        expect_('(display #\\a)').to(output('a'));
        expect_('(display #\\space)').to(output(' '));
        expect_('(display #\\newline)').to(output('\n'));
    },

    testWritePrimitivesToJs() {
        expect_('(write 42)').to(output('42'));
        expect_('(write #t)').to(output('#t'));
        expect_('(write #f)').to(output('#f'));
        expect_('(write "hello, world")').to(output('"hello, world"'));
        expect_("(write 'hello)").to(output('hello'));
        expect_('(write (quote hello))').to(output('hello'));
        expect_('(write #\\a)').to(output('#\\a'));
        expect_('(write #\\space)').to(output('#\\space'));
        expect_('(write #\\newline)').to(output('#\\newline'));
    },

    testSanityChecks() {
        expect_('(+ 1 1)').to(evalTo('2'));
        expect_('(procedure? procedure?)').to(evalTo('#t'));
        expect_('(string-append "hello " "world")').
            to(evalTo('"hello world"'));
        expect_("'a").to(evalTo('a'));
        expect_("''a").to(evalTo("'a"));
        expect_("'''a").to(evalTo("''a"));
        expect_("''''a").to(evalTo("'''a"));
        expect_("'''''a").to(evalTo("''''a"));
    },

    testReturnRecursiveTypesToJs() {
        expect_('#()').to(evalTo('#()'));
        expect_("'()").to(evalTo('()'));
        expect_("(list '() '() '() '(42))").
            to(evalTo('(() () () (42))'));
        expect_('(list 1 2 3)').to(evalTo('(1 2 3)'));
        expect_("(cons 'a (cons 'b (cons 'c '())))").
            to(evalTo('(a b c)'));
        expect_("(cons 'a 'b)").to(evalTo('(a . b)'));
    },

    testDisplayRecursiveTypesToJs() {
        expect_('(display #())').to(output('#()'));
        expect_("(display '())").to(output('()'));
        expect_("(display (list '() '() '() '(42)))").
            to(output('(() () () (42))'));
        expect_('(display (list 1 2 3))').to(output('(1 2 3)'));
        expect_("(display (cons 'a (cons 'b (cons 'c '()))))").
            to(output('(a b c)'));
        expect_("(display (cons 'a 'b))").to(output('(a . b)'));
    },

    testWriteRecursiveTypesToJs() {
        expect_('(write #())').to(output('#()'));
        expect_("(write '())").to(output('()'));
        expect_("(write (list '() '() '() '(42)))").to(output('(() () () (42))'));
        expect_('(write (list 1 2 3))').to(output('(1 2 3)'));
        expect_("(write (cons 'a (cons 'b (cons 'c '()))))").
            to(output('(a b c)'));
        expect_("(write (cons 'a 'b))").to(output('(a . b)'));
    },

    /*
     * R5RS doesn't actually forbid these external representations to be
     * the empty string, but empty strings are not helpful to return in a REPL.
     */
    testNonStandardExternalRepresentations() {
        expect_('+').not().to(evalTo(''));
        expect_('(lambda (x) x)').not().to(evalTo(''));
        expect_('(current-input-port)').not().to(evalTo(''));
        expect_('(current-output-port)').not().to(evalTo(''));
        expect_('(scheme-report-environment 5)').
            not().to(evalTo(''));
        expect_('(null-environment 5)').not().to(evalTo(''));
    },

    testUnspecifiedReturnValues() {
        expect_('').to(evalTo(''));
        expect_(' ').to(evalTo(''));
        expect_('\n').to(evalTo(''));
        expect_('\t').to(evalTo(''));
        expect_('    \t \n\n\n   ').to(evalTo(''));
        expect_('(define x 1)').to(evalTo(''));
        expect_('(define x 1) (set! x 2)').to(evalTo(''));
        expect_('(define x (cons 1 2)) (set-car! x x)').to(evalTo(''));
        expect_('(define x (cons 1 2)) (set-cdr! x x)').to(evalTo(''));
        expect_('(if #f #t)').to(evalTo(''));
        expect_('(write "foo")').to(evalTo(''));
        expect_('(display 42)').to(evalTo(''));
        expect_('(write-char #\\a)').to(evalTo(''));
        expect_('(close-input-port (current-input-port))').
            to(evalTo(''));
        expect_('(close-input-port (open-input-file "foo"))').
            to(evalTo(''));
        expect_('(close-output-port (open-output-file "foo"))').
            to(evalTo(''));
        expect_('(close-output-port (current-output-port))').
            to(evalTo(''));
    },

    testErrors() {
        expect_('(').to(Throw(new Error(ErrorType.READ, `read error: ${LPAREN}`)));
        expect_(')').to(Throw(new Error(ErrorType.READ, `read error: ${RPAREN}`)));
        expect_('(eval)').to(Throw(Error.incorrectNumArgs('eval', 2, 0)));
        expect_('(eval 1 2 3 4 5)').
            to(Throw(Error.incorrectNumArgs('eval', 2, 5)));
        expect_('(let ((foo (lambda (x) x))) (foo))').
            to(Throw(Error.incorrectNumArgs(''/* TODO bl lambda */, 1, 0)));
        expect_('(let ((foo (lambda (x) x))) (foo 1 2))').
            to(Throw(Error.incorrectNumArgs('' /* TODO bl lambda */, 1, 2)));
        expect_("(set-car! '(1 2 3) 4)").to(Throw(Error.immutable('')));
        expect_('(let ((g (lambda () "***"))) (string-set! (g) 0 #\\?))').
            to(Throw(Error.immutable(''))); // Example from R5RS 6.3.5
        expect_("(string-set! (symbol->string 'immutable) 0 #\\?)").
            to(Throw(Error.immutable(''))); // Example from R5RS 6.3.5
        expect_("(vector-set! '#(0 1 2) 1 \"doe\")").
            to(Throw(Error.immutable(''))); // Example from R5RS 6.3.6
        expect_('(make-vector)').
            to(Throw(Error.tooFewVarargs('make-vector', 1, 0)));
        expect_('(make-vector 1 2 3 4 5)').
            to(Throw(Error.tooManyVarargs('make-vector', 2, 5)));
        expect_('(let ((foo (lambda (x . y) x))) (foo))').
            to(Throw(Error.tooFewVarargs('', 1, 0)));
        expect_('(+ "a" "b")').
            to(Throw(argumentTypeError(
                'a', 0, '+', Types.NUMBER, Types.STRING)));
        expect_('(scheme-report-environment 6)').
            to(Throw(Error.unimplementedOption('')));
        expect_('(null-environment 6)').
            to(Throw(Error.unimplementedOption('')));
    }
});
