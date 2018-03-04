goog.module('r5js.boot');

const Environment = goog.require('r5js.Environment');
const Evaluator = goog.require('r5js.Evaluator');
const {InputPort, NULL_INPUT_PORT} = require('/js/io/io_collect_es6_sources.es6/node_modules/__main__/js/io/input_port');
const ParserImpl = goog.require('r5js.ParserImpl');
const Pipeline = goog.require('r5js.Pipeline');
const PrimitiveProcedures = goog.require('r5js.PrimitiveProcedures');
const {Reader} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/reader');
const {TokenStream} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/token_stream');
const trampoline = goog.require('r5js.trampoline');
const {Datum, ProcCallLike} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {OutputPort, NULL_OUTPUT_PORT} = require('/js/io/io_collect_es6_sources.es6/node_modules/__main__/js/io/output_port');

/**
 * The main bootstrap function. Given Scheme source code for R5RS syntax and
 * procedures, returns an interpreter that is ready to run on user input,
 * and is connected to the given ports.
 * @param {string} syntaxLib Scheme source code for the R5RS syntax library.
 * @param {string} procLib Scheme source code for the R5RS procedure library.
 * @param {!InputPort=} inputPort Input port that the new evaluator will be connected to.
 * @param {!OutputPort=} outputPort Output port that the new evaluator will be connected to.
 * @return {!Evaluator}
 */
function boot(syntaxLib, procLib, inputPort=NULL_INPUT_PORT, outputPort=NULL_OUTPUT_PORT) {
  const nullEnv = new Environment(null /* enclosingEnv */);
  installSchemeSource(syntaxLib, nullEnv);
  nullEnv.seal();

  /* r5RSEnv is the normal "root" environment. But we also have to
     support the "null environment", which is just the R5RS required syntax
     (no procedures). Example:

     (eval + (null-environment 5)) => +
     (eval '+ (null-environment 5)) => error (+ not defined)

     The easiest way to do this would be to put all the syntax definitions
     in nullEnv, all the procedure definitions in r5RSEnv, and
     set r5RSEnv.enclosingEnv = nullEnv. Unfortunately, macros
     require backlinks to their enclosing environments to resolve free
     identifiers correctly. If the macros are defined in the procedures'
     parent environment, things like

     (let ((x 1)) (+ x x))

     will fail, since + is defined in r5RSEnv, which is unreachable
     from nullEnv. So we make the null environment completely
     separate, and manually copy the bindings into r5RSEnv
     (remembering to clone the macros and set their backlinks correctly).
     Ugh. */

  const r5RSEnv = nullEnv.clone();
  PrimitiveProcedures.install(nullEnv, r5RSEnv);
  installSchemeSource(procLib, r5RSEnv);
  r5RSEnv.seal();
  return new Evaluator(new Pipeline(r5RSEnv), inputPort, outputPort);
}


/**
 * @param {string} lib Scheme source code.
 * @param {!IEnvironment} env Environment to install the source code's definitions into.
 */
function installSchemeSource(lib, env) {
    const continuable = /** @type {!ProcCallLike} */ (new ParserImpl.ParserImpl(
        /** @type {!Datum} */ (Reader.forTokenStream(
            TokenStream.forText(lib)).read())).parse().desugar(env));
    trampoline(continuable, env, NULL_INPUT_PORT, NULL_OUTPUT_PORT);
}

exports = {boot};
