/* Copyright 2011-2014 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */


goog.provide('r5js.boot');


goog.require('r5js.Environment');
goog.require('r5js.InputPort');
goog.require('r5js.OutputPort');
goog.require('r5js.ParserImpl');
goog.require('r5js.PipelineImpl');
goog.require('r5js.PrimitiveProcedures');
goog.require('r5js.ReaderImpl');
goog.require('r5js.Scanner');
goog.require('r5js.sync.EvaluatorImpl');
goog.require('r5js.trampoline');


/**
 * The main bootstrap function. Given Scheme source code for R5RS syntax and
 * procedures, returns an interpreter that is ready to run on user input,
 * and is connected to the given ports.
 * @param {string} syntaxLib Scheme source code for the R5RS syntax library.
 * @param {string} procLib Scheme source code for the R5RS procedure library.
 * @param {!r5js.InputPort=} opt_inputPort Optional input port that the new
 * evaluator will be connected to. If not given, defaults to
 * {@link r5js.InputPort.NULL}.
 * @param {!r5js.OutputPort=} opt_outputPort Optional output port that the new
 * evaluator will be connected to. If not given, defaults to
 * {@link r5js.OutputPort.NULL}.
 * @return {!r5js.sync.Evaluator}
 */
r5js.boot = function(
    syntaxLib, procLib, opt_inputPort, opt_outputPort) {
  var nullEnv = new r5js.Environment(null /* enclosingEnv */);
  r5js.boot.installSchemeSource_(syntaxLib, nullEnv);
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

  var r5RSEnv = nullEnv.clone();
  r5js.PrimitiveProcedures.install(nullEnv, r5RSEnv);
  r5js.boot.installSchemeSource_(procLib, r5RSEnv);
  r5RSEnv.seal();
  return new r5js.sync.EvaluatorImpl(
      new r5js.PipelineImpl(r5RSEnv),
      opt_inputPort || r5js.InputPort.NULL,
      opt_outputPort || r5js.OutputPort.NULL);
};


/**
 * @param {string} lib Scheme source code.
 * @param {!r5js.IEnvironment} env Environment to install the source code's
 * definitions into.
 * @private
 */
r5js.boot.installSchemeSource_ = function(lib, env) {
  var continuable = /** @type {!r5js.ProcCallLike} */ (new r5js.ParserImpl(
      /** @type {!r5js.Datum} */ (new r5js.ReaderImpl(
      new r5js.Scanner(lib)).read())).parse().desugar(env));
  r5js.trampoline(continuable, env, r5js.InputPort.NULL, r5js.OutputPort.NULL);
};
