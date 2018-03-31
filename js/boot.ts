import {InputPort, NULL_INPUT_PORT} from "./io/input_port";
import {NULL_OUTPUT_PORT, OutputPort} from "./io/output_port";
import {Evaluator} from "./eval/evaluator";
import {EnvironmentImpl} from "./runtime/environment_impl";
import {install} from "./runtime/primitive_procedures";
import {Pipeline} from "./eval/pipeline";
import {ParserImpl} from "./parse/parser_impl";
import {newReader, Reader} from "./read/reader";
import {newTokenStream, TokenStream} from "./scan/token_stream";
import {trampoline} from "./runtime/trampoline";
import {Environment} from "./runtime/environment";

/**
 * The main bootstrap function. Given Scheme source code for R5RS syntax and procedures, returns
 * an interpreter that is ready to run on user input, and is connected to the given ports.
 * @param syntaxLib Scheme source code for the R5RS syntax library.
 * @param procLib Scheme source code for the R5RS procedure library.
 * @param inputPort Input port that the new evaluator will be connected to.
 * @param outputPort Output port that the new evaluator will be connected to.
 */
export function boot(
    syntaxLib: string,
    procLib: string,
    inputPort: InputPort = NULL_INPUT_PORT,
    outputPort: OutputPort = NULL_OUTPUT_PORT): Evaluator {
  const nullEnv = new EnvironmentImpl(null /* enclosingEnv */);
  installSchemeSource(syntaxLib, nullEnv);
  nullEnv.seal();

  // r5RSEnv is the normal "root" environment. But we also have to support the "null environment",
  // which is just the R5RS required syntax (no procedures). Example:
  //
  // (eval + (null-environment 5)) => +
  // (eval '+ (null-environment 5)) => error (+ not defined)
  //
  // The easiest way to do this would be to put all the syntax definitions in nullEnv, all the
  // procedure definitions in r5RSEnv, and set r5RSEnv.enclosingEnv = nullEnv. Unfortunately, macros
  // require backlinks to their enclosing environments to resolve free identifiers correctly. If the
  // macros are defined in the procedures' parent environment, things like
  //
  // (let ((x 1)) (+ x x))
  //
  // will fail, since + is defined in r5RSEnv, which is unreachable from nullEnv. So we make the
  // null environment completely separate, and manually copy the bindings into r5RSEnv (remembering
  // to clone the macros and set their backlinks correctly). Ugh.

  const r5RSEnv = nullEnv.clone();
  install(nullEnv, r5RSEnv);
  installSchemeSource(procLib, r5RSEnv);
  r5RSEnv.seal();
  return new Evaluator(new Pipeline(r5RSEnv), inputPort, outputPort);
}


function installSchemeSource(lib: string, env: Environment) {
  const continuable = new ParserImpl(
      newReader(
          newTokenStream(lib)).read()).parse()!.desugar(env);
  trampoline(continuable, env, NULL_INPUT_PORT, NULL_OUTPUT_PORT);
}