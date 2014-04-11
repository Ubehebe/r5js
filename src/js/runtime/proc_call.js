goog.provide('r5js.ProcCall');
goog.provide('r5js.newProcCall');

goog.require('r5js.ContinuableHelper');
goog.require('r5js.Continuation');
goog.require('r5js.Datum');
goog.require('r5js.Environment');
goog.require('r5js.GeneralSyntaxError');
goog.require('r5js.IllegalEmptyApplication');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.JsObjOrMethod');
goog.require('r5js.Macro');
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
goog.require('r5js.datumutil');
goog.require('r5js.parse.Terminals');
goog.require('r5js.runtime.UNSPECIFIED_VALUE');



/**
 * @param {!r5js.ast.Identifier} operatorName
 * @param {?} firstOperand
 * @struct
 * @constructor
 */
r5js.ProcCall = function(operatorName, firstOperand) {

  /** @const @private */ this.operatorName_ = operatorName;

  /** @const @protected {?} */
  this.firstOperand = firstOperand;

  /** @protected {r5js.IEnvironment} */ this.env = null;
};


/** @return {?} TODO bl. */
r5js.ProcCall.prototype.getFirstOperand = function() {
  return this.firstOperand;
};


/** @return {r5js.IEnvironment} */
r5js.ProcCall.prototype.getEnv = function() {
  return this.env;
};


/**
 * @param {!r5js.IEnvironment} env An environment to use.
 * @param {boolean=} opt_override True iff the ProcCall's own environment
 * should be overridden.
 */
r5js.ProcCall.prototype.setEnv = function(env, opt_override) {
  if (this.env && !opt_override)
    throw new r5js.InternalInterpreterError('invariant incorrect');
  this.env = env;
};


/**
 * If the ProcCall already has an environment, don't overwrite it.
 * Exception: if this is a continuation "escape call", we do overwrite it.
 * This exception was prompted by things like
 *
 * (call-with-current-continuation
 *   (lambda (exit)
 *     (for-each
 *       (lambda (x)
 *         (if (negative? x) (exit x)))
 *   '(54 0 37 -3 245 19)) #t))
 *
 * At the end of each invocation of (lambda (x) ...), the environment on
 * (exit x) should be updated to reflect the most recent binding of x.
 * Otherwise, the trampoline would see that -3 is negative, but the x in
 * (exit x) would still be bound to 54.
 *
 * This is quite ad-hoc and could contain bugs.
 *
 * @param {!r5js.IEnvironment} env An environment.
 */
r5js.ProcCall.prototype.maybeSetEnv = function(env) {
  if (!this.env) {
    this.env = env;
  }
};


/** TODO bl document */
r5js.ProcCall.prototype.clearEnv = function() {
  this.env = null;
};


/**
 * Just for debugging.
 * @param {!r5js.Continuation} continuation
 * @param {number} indentLevel
 * @param {boolean} suppressEnv
 * @return {string}
 */
r5js.ProcCall.prototype.debugString = function(
    continuation, indentLevel, suppressEnv) {
  var ans = '\n';
  for (var i = 0; i < indentLevel; ++i)
    ans += '\t';
  ans += '(' + this.operatorName_.getPayload();
  if (this.env && !suppressEnv)
    ans += '|' + this.env;
  if (this.operatorName_) {
    for (var cur = this.firstOperand; cur; cur = cur.getNextSibling()) {
      ans += ' ' + cur.toString();
    }
  } else {
    ans += ' ' + this.firstOperand;
  }
  if (continuation)
    ans += ' ' + continuation.debugString(indentLevel + 1);
  return ans + ')';
};


/**
 * @return {!r5js.Datum}
 * @private
 */
r5js.ProcCall.prototype.reconstructDatum_ = function() {
  var op = new r5js.ast.Identifier(this.operatorName_.getPayload());
  op.setNextSibling(this.firstOperand);
  return new r5js.SiblingBuffer().appendSibling(op).toList(r5js.ast.List);
};


/**
 * @return {boolean} True iff the operands are in continuation-passing style.
 */
r5js.ProcCall.prototype.operandsInCpsStyle = function() {
  for (var cur = this.firstOperand; cur; cur = cur.nextSibling_) {
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
 * @param {!r5js.Continuation} continuation
 * @param {!r5js.TrampolineHelper} resultStruct
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 */
r5js.ProcCall.prototype.cpsify = function(
    continuation, resultStruct, parserProvider) {

  var newCallChain = new r5js.ContinuableHelper();
  var finalArgs = new r5js.SiblingBuffer();
  var maybeContinuable;

  for (var arg = this.firstOperand; arg; arg = arg.getNextSibling()) {
    arg.resetDesugars();
    if (arg instanceof r5js.ast.Quote) {
      finalArgs.appendSibling(arg.clone(null /* parent */));
    } else if (arg instanceof r5js.ast.Quasiquote) {
      if ((maybeContinuable =
          arg.processQuasiquote(
          /** @type {!r5js.IEnvironment} */ (this.env),
          continuation.lastResultName,
          parserProvider)) instanceof r5js.Continuable) {
        finalArgs.appendSibling(
            new r5js.ast.Identifier(maybeContinuable
                        .getLastContinuable()
                        .continuation
                        .lastResultName));
        newCallChain.appendContinuable(
            /** @type {!r5js.Continuable} */(maybeContinuable));
      } else {
        /* R5RS 4.2.6: "If no commas appear within the <qq template>,
                 the result of evaluating `<qq template> is equivalent to
                 the result of evaluating '<qq template>." We implement this
                 merely by switching the type of the datum from quasiquote (`)
                 to quote ('). evalArgs will see the quote and evaluate it
                 accordingly. */
        finalArgs.appendSibling(arg);
      }
    } else if (arg instanceof r5js.ast.Lambda) {
      finalArgs.appendSibling(
          new r5js.ast.Identifier(/** @type {string} */ (arg.getName())));
    } else if (arg.isImproperList()) {
      throw new r5js.GeneralSyntaxError(arg);
    } else if ((maybeContinuable = arg.desugar(
        /** @type {!r5js.IEnvironment} */ (this.env))) instanceof
            r5js.Continuable) {
      /* todo bl is it an invariant violation to be a list
             and not to desugar to a Continuable? */
      finalArgs.appendSibling(
          new r5js.ast.Identifier(maybeContinuable.
          getLastContinuable().continuation.lastResultName));
      newCallChain.appendContinuable(maybeContinuable);
    } else {
      var clonedArg = arg.clone(null /* parent */);
      if (clonedArg instanceof r5js.ast.CompoundDatum) {
        clonedArg.clearFirstChild();
      }
      clonedArg.setNextSibling(null);
      finalArgs.appendSibling(clonedArg);
    }
  }

  newCallChain.appendContinuable(
      r5js.newProcCall(
      this.operatorName_,
      finalArgs.toSiblings(),
      new r5js.Continuation()));

  var ans = newCallChain.toContinuable();
  ans.setStartingEnv(/** @type {!r5js.IEnvironment} */ (this.env));
  var lastContinuable = ans.getLastContinuable();
  lastContinuable.continuation = continuation;
  resultStruct.nextContinuable = ans;
};


/**
 * @param {!r5js.Macro} macro The macro.
 * @param {!r5js.Continuation} continuation A continuation.
 * @param {!r5js.TrampolineHelper} resultStruct The trampoline helper.
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 * @private
 */
r5js.ProcCall.prototype.tryMacroUse_ = function(
    macro, continuation, resultStruct, parserProvider) {

  var newEnv = new r5js.Environment(this.env);
  var newParseTree = macro.transcribe(
      this.reconstructDatum_(),
      newEnv,
      parserProvider
      );

  /* Just like with tryNonPrimitiveProcedures, we have to remember when
     to jump back to the old environment. */
  if (this.env) {
    continuation.rememberEnv(this.env);
  }

  // useful for debugging
  // console.log('transcribed ' +
  // this.reconstructDatum_() +
  // ' => ' + newDatumTree);

  var newContinuable = newParseTree.desugar(newEnv, true).
      setStartingEnv(newEnv);

  newContinuable.getLastContinuable().continuation = continuation;
  resultStruct.nextContinuable = newContinuable;
};


/**
 * @param {!r5js.Continuation} continuation
 * @param {!r5js.TrampolineHelper} resultStruct
 * @param {!r5js.EnvBuffer} envBuffer
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 * @return {*}
 */
r5js.ProcCall.prototype.evalAndAdvance = function(
    continuation, resultStruct, envBuffer, parserProvider) {

  /* If the procedure call has no attached environment, we use
     the environment left over from the previous action on the trampoline. */
  if (!this.env) {
    this.setEnv(/** @type {!r5js.IEnvironment} */ (envBuffer.getEnv()));
  }

  var proc = this.env.getProcedure(/** @type {string} */ (
      this.operatorName_.getPayload()));

  if (r5js.ProcedureLike.isImplementedBy(proc)) {
    proc.evalAndAdvance(this, continuation, resultStruct, parserProvider);
  } else if (proc instanceof r5js.Macro) {
    this.tryMacroUse_(proc, continuation, resultStruct, parserProvider);
  } else if (proc instanceof r5js.Continuation) {
    this.tryContinuation_(proc, continuation, resultStruct);
  } else if (proc instanceof r5js.JsObjOrMethod) {
    this.tryFFI_(proc, continuation, resultStruct, parserProvider);
  } else {
    throw new r5js.EvalError(
        'procedure application: expected procedure, given ' +
        this.operatorName_);
  }

  /* Save the environment we used in case the next action on the trampoline
     needs it (for example branches, which have no environment of their own). */
  envBuffer.setEnv(/** @type {!r5js.IEnvironment} */(this.env));

  // We shouldn't leave the environment pointer hanging around.
  this.clearEnv();

  return resultStruct;
};


/**
 * @param {!r5js.Continuation} continuation
 * @param {!r5js.runtime.Value} val
 */
r5js.ProcCall.prototype.bindResult = function(continuation, val) {

  var name = continuation.lastResultName;
  var nextProcCall = continuation.getAdjacentProcCall();

  if (nextProcCall instanceof r5js.ProcCall) {
    var maybeEnv = nextProcCall.env;
    /* If the next procedure call already has an environment,
         bind the result there. Otherwise, bind it in the current
         environment; it will be carried forward by the EnvBuffer. */
    if (maybeEnv) {
      maybeEnv.addBinding(name, val);
    } else {
      this.env.addBinding(name, val);
    }
  }

/* If the next thing is not a procedure call, it will reuse this procedure
     call's environment, so just bind the result here. */
  else {
    this.env.addBinding(name, val);
  }
};


/**
 * @param {!r5js.Continuation} proc The continuation.
 * @param {!r5js.Continuation} continuation The following continuation.
 * @param {!r5js.TrampolineHelper} resultStruct The trampoline helper.
 * @private
 */
r5js.ProcCall.prototype.tryContinuation_ = function(
    proc, continuation, resultStruct) {
  var arg = this.evalArgs(false)[0]; // there will only be 1 arg
  this.env.addBinding(proc.lastResultName, arg);
  resultStruct.ans = arg;
  resultStruct.nextContinuable = proc.nextContinuable;

  if (proc.beforeThunk) {
    var before = proc.beforeThunk;
    var cur = proc.nextContinuable;
    before.appendContinuable(cur);
    resultStruct.nextContinuable = before;
    // todo bl is it safe to leave proc.beforeThunk defined?
  }

  /* Cut out the current proc call from the continuation chain to
     avoid an infinite loop. Example:

     (define cont #f)
     (display
     (call-with-current-continuation
     (lambda (c)
     (set! cont c)
     "inside continuation")))
     (cont "outside continuation")
     42

     This should display "inside continuation", then "outside continuation",
     then return 42. When the trampoline is at

     (cont "outside continuation")

     proc.nextContinuable will be something like

     (cont "outside continuation" _0 [_0 (id 42 [_1 ...])])

     We clearly have to cut out the first part of this chain to avoid an
     infinite loop. */
  for (var tmp = resultStruct.nextContinuable, prev;
      tmp;
      prev = tmp, tmp = tmp.continuation.nextContinuable) {
    if (tmp.subtype === this) {
      if (prev)
        prev.continuation.nextContinuable = tmp.continuation.nextContinuable;
      else
        resultStruct.nextContinuable = tmp.continuation.nextContinuable;
      break;
    }
  }
};


/**
 * @param {boolean} wrapArgs
 * @return {!Array.<!r5js.Datum>}
 * TODO bl: this method is too long.
 * @suppress {accessControls} for the raw access to nextSibling_.
 */
r5js.ProcCall.prototype.evalArgs = function(wrapArgs) {
  var args = [];

  /* Special logic for values and call-with-values. Example:

     (call-with-values (lambda () (values 1 2 3)) +)

     The "producer" procedure, (lambda () (values 1 2 3)), will desugar to
     something like

     (values 1 2 3 [_0 ...])

     In this implementation, this will bind the JavaScript array [1, 2, 3]
     to _0. Later on the trampoline, we reach (+ _0). We have to know that
     _0 refers to an array of values, not a single value. */
  if (this.firstOperand instanceof r5js.ast.Identifier &&
      !this.firstOperand.getNextSibling()) {
    var maybeArray = this.env.get(
        /** @type {string} */ (this.firstOperand.getPayload()));
    if (maybeArray instanceof Array)
      return maybeArray;
    // Otherwise, fall through to normal logic.
  }

  // todo bl too much logic
  for (var cur = this.firstOperand; cur; cur = cur.nextSibling_) {
    if (cur instanceof r5js.Continuation) {
      args.push(cur);
    } else if (cur instanceof r5js.ast.Identifier) {
      var name = /** @type {string} */ (cur.getPayload());
      var toPush = wrapArgs ?
          r5js.datumutil.maybeWrapResult(this.env.get(name)) :
          this.env.get(name);
      /* Macros are not first-class citizens in Scheme; they cannot
             be passed as arguments. Internally, however, we do just that
             for convenience. The isLetOrLetrecSyntax flag discriminates
             between the programmer and the implementation. */
      if (toPush instanceof r5js.Macro &&
          !toPush.isLetOrLetrecSyntax()) {
        throw new r5js.MacroError(
            /** @type {string} */(cur.getPayload()), 'bad syntax');
      }
      args.push(toPush);
    } else if (cur instanceof r5js.ast.Quote) {
      // the newIdOrLiteral part is for (quote quote)
      args.push(cur.getFirstChild() ?
                cur.getFirstChild() :
                new r5js.ast.Identifier(r5js.parse.Terminals.QUOTE));
    } else if (cur instanceof r5js.ast.Lambda) {
      args.push(cur);
    } else if (cur.getPayload() !== undefined) {
      var clone = cur.clone(null /* parent */);
      if (clone instanceof r5js.ast.CompoundDatum) {
        clone.clearFirstChild();
      }
      clone.setNextSibling(null);
      args.push(clone);
    } else {
      throw new r5js.InternalInterpreterError('unexpected datum ' + cur);
    }
  }

  return args;
};


/**
 * @param {?} operatorName
 * @param {?} firstOperand
 * @param {!r5js.Continuation} continuation A continuation.
 * @return {!r5js.Continuable} The new procedure call.
 */
r5js.newProcCall = function(operatorName, firstOperand, continuation) {
  return new r5js.Continuable(
      new r5js.ProcCall(operatorName, firstOperand),
      continuation);
};



