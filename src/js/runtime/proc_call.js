goog.provide('r5js.ProcCall');


goog.require('r5js.ContinuableHelper');
goog.require('r5js.Continuation');
goog.require('r5js.Datum');
goog.require('r5js.Environment');
goog.require('r5js.GeneralSyntaxError');
goog.require('r5js.IllegalEmptyApplication');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.Macro');
goog.require('r5js.ProcCallLike');
goog.require('r5js.Procedure');
goog.require('r5js.Ref');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.CompoundDatum');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.Lambda');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Literal');
goog.require('r5js.ast.Macro');
goog.require('r5js.ast.Quote');
goog.require('r5js.ast.SimpleDatum');
goog.require('r5js.ast.String');
goog.require('r5js.parse.Terminals');
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
  /** @const @protected */ this.firstOperand = firstOperand;
};
goog.inherits(r5js.ProcCall, r5js.ProcCallLike);


/** @return {?} TODO bl. */
r5js.ProcCall.prototype.getFirstOperand = function() {
  return this.firstOperand;
};


/** @suppress {checkTypes} for the assignment to null. TODO bl remove. */
r5js.ProcCall.prototype.clearEnv = function() {
  this.setStartingEnv(null);
};


/**
 * @return {!r5js.Datum}
 * @private
 */
r5js.ProcCall.prototype.reconstructDatum_ = function() {
  var op = new r5js.ast.Identifier(this.operatorName_.getPayload());
  if (this.firstOperand) {
    op.setNextSibling(this.firstOperand);
  }
  return new r5js.SiblingBuffer().appendSibling(op).toList(r5js.ast.List);
};


/**
 * @return {boolean} True iff the operands are in continuation-passing style.
 * @private
 */
r5js.ProcCall.prototype.operandsInContinuationPassingStyle_ = function() {
  for (var cur = this.firstOperand; cur; cur = cur.getNextSibling()) {
    if (cur instanceof r5js.Datum) {
      if (cur instanceof r5js.ast.List && !cur.getFirstChild()) {
        throw new r5js.IllegalEmptyApplication(this.operatorName_.getPayload());
      } else if (!(cur instanceof r5js.ast.Literal ||
          cur instanceof r5js.ast.Quote)) {
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

  for (var arg = this.firstOperand; arg; arg = arg.getNextSibling()) {
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
      throw new r5js.GeneralSyntaxError(arg);
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
  ans.setStartingEnv(/** @type {!r5js.IEnvironment} */ (this.getEnv()));
  var lastContinuable = r5js.ProcCallLike.getLast(ans);
  var next = this.getNext();
  if (next) {
    lastContinuable.setNext(next);
  }
  lastContinuable.setResultName(this.getResultName());
  trampolineHelper.setNext(ans);
};


/** @override */
r5js.ProcCall.prototype.evalAndAdvance = function(
    resultStruct, envBuffer, parserProvider) {

  var curEnv = this.getEnv();
  var bufferEnv = envBuffer.getEnv();

  /* If the procedure call has no attached environment, we use
     the environment left over from the previous action on the trampoline. */
  if (!curEnv && bufferEnv) {
    this.setStartingEnv(bufferEnv);
  }

  var proc = this.getEnv().getProcedure(/** @type {string} */ (
      this.operatorName_.getPayload()));

  if (proc instanceof r5js.Procedure) {
    if (!this.operandsInContinuationPassingStyle_()) {
      this.cpsify_(resultStruct, parserProvider);
    } else {
      var args = this.evalArgs();
      proc.evaluate(args, this, resultStruct);
    }
  } else if (proc instanceof r5js.Macro) {
    var rawDatum = this.reconstructDatum_();
    proc.evaluate(rawDatum, this, resultStruct, parserProvider);
  } else if (proc instanceof r5js.Continuation) {
    var fakeArg = this.evalArgs()[0]; // TODO bl
    proc.evaluate(fakeArg, this, resultStruct);
  } else {
    throw new r5js.EvalError(
        'procedure application: expected procedure, given ' +
        this.operatorName_);
  }

  /* Save the environment we used in case the next action on the trampoline
     needs it (for example branches, which have no environment of their own). */
  envBuffer.setEnv(/** @type {!r5js.IEnvironment} */(this.getEnv()));

  // We shouldn't leave the environment pointer hanging around.
  this.clearEnv();
};


/**
 * @return {!Array.<!r5js.runtime.Value>}
 * TODO bl: this method is too long.
 */
r5js.ProcCall.prototype.evalArgs = function() {
  var maybeArray;
  if (maybeArray = this.evalArgsCallWithValues_()) {
    return maybeArray;
  }

  var args = [];

  for (var cur = this.firstOperand; cur; cur = cur.nextSibling_) {
    if (cur instanceof r5js.ast.Identifier) {
      var name = cur.getPayload();
      var toPush = this.getEnv().get(name);
      /* Macros are not first-class citizens in Scheme; they cannot
             be passed as arguments. Internally, however, we do just that
             for convenience. The isLetOrLetrecSyntax flag discriminates
             between the programmer and the implementation. */
      if (toPush instanceof r5js.Macro &&
          !toPush.isLetOrLetrecSyntax()) {
        throw new r5js.MacroError(name, 'bad syntax');
      }
      args.push(toPush);
    } else if (cur instanceof r5js.ast.Quote) {
      args.push(cur.getFirstChild());
    } else if (cur instanceof r5js.ast.Lambda) {
      args.push(cur);
    } else if (cur instanceof r5js.ast.SimpleDatum) {
      args.push(cur.clone(null /* parent */));
    } else {
      throw new r5js.InternalInterpreterError('unexpected datum ' + cur);
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
 * @return {Array.<!r5js.runtime.Value>}
 * @private
 */
r5js.ProcCall.prototype.evalArgsCallWithValues_ = function() {
  if (this.firstOperand instanceof r5js.ast.Identifier &&
      !this.firstOperand.getNextSibling()) {
    var maybeArray = this.getEnv().get(
        /** @type {string} */ (this.firstOperand.getPayload()));
    if (maybeArray instanceof Array) {
      return maybeArray;
    }
  }
  return null;
};


