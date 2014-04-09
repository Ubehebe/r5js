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


goog.provide('r5js.procs');


goog.require('r5js.Continuable');
goog.require('r5js.ContinuableHelper');
goog.require('r5js.Continuation');
goog.require('r5js.Datum');
goog.require('r5js.DatumType');
// TODO bl circular dependency goog.require('r5js.Environment');
goog.require('r5js.EvalError');
goog.require('r5js.FFIError');
goog.require('r5js.IllegalEmptyApplication');
goog.require('r5js.IncorrectNumArgs');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.JsObjOrMethod');
goog.require('r5js.IEnvironment');
goog.require('r5js.ast.CompoundDatum');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.Lambda');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Quote');
goog.require('r5js.Macro');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.MacroError');
goog.require('r5js.Procedure');
goog.require('r5js.PrimitiveProcedure');
goog.require('r5js.ast.CompoundDatum');
goog.require('r5js.ast.Quasiquote');
goog.require('r5js.QuasiquoteError');
goog.require('r5js.ProcCall');
goog.require('r5js.ast.Quote');
goog.require('r5js.ast.SimpleDatum');
goog.require('r5js.datumutil');
goog.require('r5js.Ref');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.TooFewArgs');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.Literal');
goog.require('r5js.runtime.UNSPECIFIED_VALUE');
goog.require('r5js.datumutil');


/**
 * Utility functions for dealing with procedures and procedure calls.
 */
r5js.procs = {};


/**
 * @param {string} dstName
 * @param {string} srcName
 * @param {!r5js.Continuation} continuation
 * @return {!r5js.Continuable}
 */
r5js.procs.newAssignment = function(dstName, srcName, continuation) {
  var operands = new r5js.SiblingBuffer()
        .appendSibling(new r5js.ast.Identifier(dstName))
        .appendSibling(new r5js.ast.Identifier(srcName))
        .toSiblings();

  return r5js.procs.newProcCall(
      r5js.ProcCall.prototype.specialOps._set,
      operands,
      continuation
  );
};


/**
 * @param {?} operatorName
 * @param {?} firstOperand
 * @param {!r5js.Continuation} continuation A continuation.
 * @return {!r5js.Continuable} The new procedure call.
 */
r5js.procs.newProcCall = function(operatorName, firstOperand, continuation) {
  return new r5js.Continuable(
      new r5js.ProcCall(operatorName, firstOperand),
      continuation
  );
};


/**
 * If a nonterminal in the grammar has no associated desugar function,
 * desugaring it will be a no-op. That is often the right behavior,
 * but sometimes we would like to wrap the datum in a Continuable
 * object for convenience on the trampoline. For example, the program
 * "1 (+ 2 3)" should be desugared as (id 1 [_0 (+ 2 3 [_1 ...])]).
 *
 * We represent these id shims as ProcCalls whose operatorNames are null
 * and whose firstOperand is the payload.
 *
 * @param {?} payload
 * @param {string=} opt_continuationName Optional name of the continuation.
 * @return {!r5js.Continuable} The new procedure call.
 */
function newIdShim(payload, opt_continuationName) {
  return r5js.procs.newProcCall(
      r5js.ProcCall.prototype.specialOps._id,
      payload,
      new r5js.Continuation(opt_continuationName));
}