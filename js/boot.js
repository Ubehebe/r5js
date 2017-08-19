goog.module('r5js.boot');
goog.module.declareLegacyNamespace();

const Datum = goog.require('r5js.Datum');
const Environment = goog.require('r5js.Environment');
const Evaluator = goog.require('r5js.sync.Evaluator');
const IEnvironment = goog.require('r5js.IEnvironment');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const ParserImpl = goog.require('r5js.ParserImpl');
const Pipeline = goog.require('r5js.Pipeline');
const PrimitiveProcedures = goog.require('r5js.PrimitiveProcedures');
const ProcCallLike = goog.require('r5js.ProcCallLike');
const Reader = goog.require('r5js.Reader');
const TokenStream = goog.require('r5js.TokenStream');
const trampoline = goog.require('r5js.trampoline');

/**
 * The main bootstrap function. Given Scheme source code for R5RS syntax and
 * procedures, returns an interpreter that is ready to run on user input,
 * and is connected to the given ports.
 * @param {string} syntaxLib Scheme source code for the R5RS syntax library.
 * @param {string} procLib Scheme source code for the R5RS procedure library.
 * @param {!InputPort=} opt_inputPort Optional input port that the new
 * evaluator will be connected to. If not given, defaults to
 * {@link r5js.InputPort.NULL}.
 * @param {!OutputPort=} opt_outputPort Optional output port that the new
 * evaluator will be connected to. If not given, defaults to
 * {@link r5js.OutputPort.NULL}.
 * @return {!Evaluator}
 */
function boot(syntaxLib, procLib, opt_inputPort, opt_outputPort) {
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
  return Evaluator.create(
      Pipeline.forEnvironment(r5RSEnv),
      opt_inputPort || InputPort.NULL,
      opt_outputPort || OutputPort.NULL);
}


/**
 * @param {string} lib Scheme source code.
 * @param {!IEnvironment} env Environment to install the source code's definitions into.
 */
function installSchemeSource(lib, env) {
    const continuable = /** @type {!ProcCallLike} */ (new ParserImpl.ParserImpl(
        /** @type {!Datum} */ (Reader.forTokenStream(
            TokenStream.forText(lib)).read())).parse().desugar(env));
    trampoline(continuable, env, InputPort.NULL, OutputPort.NULL);
}

exports = boot;
