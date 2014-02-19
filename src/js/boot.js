/* Copyright 2011, 2012 Brendan Linn

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
goog.require('r5js.JsObjOrMethod');
goog.require('r5js.Parser');
goog.require('r5js.PrimitiveProcedures');
goog.require('r5js.Reader');
goog.require('r5js.RootEnvironment');
goog.require('r5js.scan.TokenStreamImpl');
goog.require('r5js.ScannerImpl');
goog.require('r5js.ffi');
goog.require('r5js.ffiutil');
goog.require('r5js.trampoline');


/**
 * @param {string} syntaxLib Scheme source code for the R5RS syntax library.
 * @param {string} procLib Scheme source code for the R5RS procedure library.
 * @param {!r5js.util.Logger} logger Logger for debug output.
 * @return {!r5js.IEnvironment} The R5RS environment.
 */
r5js.boot = function(syntaxLib, procLib, logger) {
  var nullEnv = new r5js.Environment('null-environment-5', null);
  install(syntaxLib, nullEnv, logger);
  nullEnv.seal();

  logger.info('installed syntax lib ok');

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

  var r5RSEnv = new r5js.RootEnvironment(
      nullEnv.clone('scheme-report-environment-5'));
  r5js.PrimitiveProcedures.install(nullEnv, r5RSEnv, logger);
  logger.info('installed primitive procedures ok');
  install(procLib, r5RSEnv, logger);
  logger.info('installed library procedures ok');
  r5RSEnv.seal();
  logger.info('interpreter is ready');
  return r5RSEnv;
};


/**
 * @param {string} lib Scheme source code.
 * @param {!r5js.IEnvironment} env Environment to install the source code's
 * definitions into.
 * @param {!r5js.util.Logger} logger Logger.
 * @return {?}
 */
function install(lib, env, logger) {
  return r5js.trampoline(
      new r5js.Parser(
      /** @type {!r5js.Datum} */ (new r5js.Reader(
      new r5js.scan.TokenStreamImpl(new r5js.ScannerImpl(lib))
      ).read())
      ).parse(null)
            .desugar(env).setStartingEnv(env),
      null,
      null,
      logger);
}
