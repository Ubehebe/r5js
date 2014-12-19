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

goog.provide('r5js.ProcCall');


goog.require('r5js.ContinuableHelper');
goog.require('r5js.Continuation');
goog.require('r5js.Datum');
goog.require('r5js.Macro');
goog.require('r5js.ProcCallLike');
goog.require('r5js.Procedure');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.CompoundDatum');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.Lambda');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Literal');
goog.require('r5js.ast.Quote');
goog.require('r5js.ast.Vector');
goog.require('r5js.error');
goog.require('r5js.runtime.UNSPECIFIED_VALUE');



/**
 * @param {!r5js.ast.Identifier} operatorName
 * @param {r5js.Datum} firstOperand
 * @param {string=} opt_lastResultName Optional name to use for the last result.
 *     If not given, a unique name will be created.
 * @extends {r5js.ProcCallLike}
 * @struct
 * @constructor
 */
r5js.ProcCall = function(operatorName, firstOperand, opt_lastResultName) {
  goog.base(this, opt_lastResultName);

  /** @const @private */ this.operatorName_ = operatorName;
  /** @const @private */ this.firstOperand_ = firstOperand;
};
goog.inherits(r5js.ProcCall, r5js.ProcCallLike);


/** @return {?} TODO bl. */
r5js.ProcCall.prototype.getFirstOperand = function() {
  return this.firstOperand_;
};


/**
 * @return {!r5js.Datum}
 * @private
 */
r5js.ProcCall.prototype.reconstructDatum_ = function() {
  var op = new r5js.ast.Identifier(this.operatorName_.getPayload());
  if (this.firstOperand_) {
    op.setNextSibling(this.firstOperand_);
  }
  return new r5js.SiblingBuffer().appendSibling(op).toList(r5js.ast.List);
};


/**
 * @return {boolean} True iff the operands are in continuation-passing style.
 * @private
 */
r5js.ProcCall.prototype.operandsInContinuationPassingStyle_ = function() {
  for (var cur = this.firstOperand_; cur; cur = cur.getNextSibling()) {
    if (cur instanceof r5js.Datum) {
      if (cur instanceof r5js.ast.List && !cur.getFirstChild()) {
        throw r5js.error.illegalEmptyApplication(
            /** @type {string} */ (this.operatorName_.getPayload()));
      } else if (!(cur instanceof r5js.ast.Literal ||
          cur instanceof r5js.ast.Quote ||
          cur instanceof r5js.ast.Vector)) {
        return false;
      }
    }
  }
  return true;
};


/**
 * If the operator resolves as a primitive or non-primitive procedure,
 * check that the operands are simple. If they're not, rearrange the flow
 * of control to compute them first.
 *
 * Example: (+ (* 2 3) (/ 4 5)) will need to be turned into something like
 *
 * (* 2 3 [_0 (/ 4 5 [_1 (+ _0 _1 [...])])])
 *
 * (We do _not_ do this if the operator resolves as a macro. Macros
 * get their arguments as unevaluated datums.)
 *
 * @param {!r5js.TrampolineHelper} trampolineHelper
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 * @suppress {checkTypes} TODO bl
 * @private
 */
r5js.ProcCall.prototype.cpsify_ = function(trampolineHelper, parserProvider) {

  var newCallChain = new r5js.ContinuableHelper();
  var finalArgs = new r5js.SiblingBuffer();
  var maybeContinuable;

  for (var arg = this.firstOperand_; arg; arg = arg.getNextSibling()) {
    arg.resetDesugars();
    if (arg instanceof r5js.ast.Quote) {
      finalArgs.appendSibling(arg.clone(null /* parent */));
    } else if (arg instanceof r5js.ast.Quasiquote) {
      maybeContinuable = arg.processQuasiquote(
          /** @type {!r5js.IEnvironment} */ (this.getEnv()),
          this.getResultName(), parserProvider);
      finalArgs.appendSibling(
          new r5js.ast.Identifier(r5js.ProcCallLike.getLast(
          maybeContinuable).getResultName()));
      newCallChain.appendProcCallLike(maybeContinuable);
    } else if (arg.isImproperList()) {
      throw r5js.error.internalInterpreterError('TODO bl');
    } else if ((maybeContinuable = arg.desugar(
        /** @type {!r5js.IEnvironment} */ (this.getEnv()))).evalAndAdvance) {
      /* todo bl is it an invariant violation to be a list
             and not to desugar to a Continuable? */
      finalArgs.appendSibling(
          new r5js.ast.Identifier(r5js.ProcCallLike.getLast(
              maybeContinuable).getResultName()));
      newCallChain.appendProcCallLike(maybeContinuable);
    } else {
      var clonedArg = arg.clone(null /* parent */);
      if (clonedArg instanceof r5js.ast.CompoundDatum) {
        clonedArg.clearFirstChild();
      }
      finalArgs.appendSibling(clonedArg);
    }
  }

  newCallChain.appendProcCallLike(
      new r5js.ProcCall(this.operatorName_, finalArgs.toSiblings()));

  var ans = newCallChain.toContinuable();
  var lastContinuable = r5js.ProcCallLike.getLast(ans);
  var next = this.getNext();
  if (next) {
    lastContinuable.setNext(next);
  }
  lastContinuable.setResultName(this.getResultName());
  trampolineHelper.setNext(ans);
};


/**
 * @override
 * @suppress {accessControls} for
 * {@link r5js.PrimitiveProcedures#getActualType_}.
 */
r5js.ProcCall.prototype.evalAndAdvance = function(
    resultStruct, env, parserProvider) {
  var proc = this.getEnv().getProcedure(/** @type {string} */ (
      this.operatorName_.getPayload()));

  if (proc instanceof r5js.Procedure) {
    if (!this.operandsInContinuationPassingStyle_()) {
      this.cpsify_(resultStruct, parserProvider);
    } else {
      var args = this.evalArgs();
      proc.evaluate(args, this, resultStruct, env);
    }
  } else if (proc instanceof r5js.Macro) {
    var rawDatum = this.reconstructDatum_();
    proc.evaluate(rawDatum, this, resultStruct, parserProvider);
  } else if (proc instanceof r5js.Continuation) {
    var fakeArg = this.evalArgs()[0]; // TODO bl
    proc.evaluate(fakeArg, this, resultStruct);
  } else {
    throw r5js.error.notAProcedure(
        this.operatorName_.getPayload(),
        r5js.PrimitiveProcedures.getActualType_(
            /** @type {!r5js.runtime.Value} */ (proc)));
  }
};


/**
 * @return {!Array<!r5js.runtime.Value>}
 * TODO bl: this method is confused.
 */
r5js.ProcCall.prototype.evalArgs = function() {
  var maybeArray;
  if (maybeArray = this.evalArgsCallWithValues_()) {
    return maybeArray;
  }

  var args = [];

  for (var cur = this.firstOperand_; cur; cur = cur.getNextSibling()) {
    if (cur instanceof r5js.ast.Identifier) {
      var name = cur.getPayload();
      var toPush = this.getEnv().get(name);
      /* Macros are not first-class citizens in Scheme; they cannot
             be passed as arguments. Internally, however, we do just that
             for convenience. The isLetOrLetrecSyntax flag discriminates
             between the programmer and the implementation. */
      if (toPush instanceof r5js.Macro &&
          !toPush.isLetOrLetrecSyntax()) {
        throw r5js.error.macro(name, 'bad syntax');
      }
      // TODO bl this doesn't seem like the right behavior. Investigate.
      args.push(toPush === null ? r5js.runtime.UNSPECIFIED_VALUE : toPush);
    } else if (cur instanceof r5js.ast.Quote) {
      args.push(cur.getFirstChild());
    } else if (cur instanceof r5js.ast.Lambda) {
      args.push(cur);
    } else if (cur instanceof r5js.Datum) {
      args.push(cur.clone(null /* parent */));
    } else {
      throw r5js.error.internalInterpreterError('unexpected datum ' + cur);
    }
  }

  return args;
};


/**
 * Special logic for values and call-with-values. Example:
 *
 * (call-with-values (lambda () (values 1 2 3)) +)
 *
 * The "producer" procedure, (lambda () (values 1 2 3)), will desugar to
 * something like
 *
 * (values 1 2 3 [_0 ...])
 *
 * In this implementation, this will bind the JavaScript array [1, 2, 3] to _0.
 * Later on the trampoline, we reach (+ _0). We have to know that _0 refers
 * to an array of values, not a single value.
 *
 * @return {Array<!r5js.runtime.Value>}
 * @private
 */
r5js.ProcCall.prototype.evalArgsCallWithValues_ = function() {
  if (this.firstOperand_ instanceof r5js.ast.Identifier &&
      !this.firstOperand_.getNextSibling()) {
    var maybeArray = this.getEnv().get(
        /** @type {string} */ (this.firstOperand_.getPayload()));
    if (maybeArray instanceof Array) {
      return maybeArray;
    }
  }
  return null;
};


