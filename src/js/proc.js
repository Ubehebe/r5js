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


goog.provide('r5js.ProcCall');
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
 * @param {?} operatorName
 * @param {?} firstOperand
 * TODO bl: operatorName is an identifier _datum_...I think
 * some call sites might be passing in strings...
 * @constructor
 */
r5js.ProcCall = function(operatorName, firstOperand) {

  /**
     * An identifier.
     * @type {?} TODO bl
     */
  this.operatorName = operatorName;

  /**
     * Identifiers or self-evaluating forms.
     * @type {?}
     */
  this.firstOperand = firstOperand;
};


/**
 * @type {r5js.IEnvironment}
 */
r5js.ProcCall.prototype.env;


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
  ans += '(' + (this.operatorName instanceof r5js.ast.SimpleDatum ?
      this.operatorName.getPayload() :
      this.specialOps.names[this.operatorName]);
  if (this.env && !suppressEnv)
    ans += '|' + this.env;
  if (this.operatorName) {
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
 */
r5js.ProcCall.prototype.reconstructDatum = function() {
  var op = new r5js.ast.Identifier(this.operatorName.getPayload());
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
        throw new r5js.IllegalEmptyApplication(this.operatorName.getPayload());
      } else if (!(cur instanceof r5js.ast.Literal ||
          cur instanceof r5js.ast.Quote)) {
        return false;
      }
    }
  }
  return true;
};


/**
 * @param {!r5js.Continuation} continuation A continuation.
 * @param {!r5js.TrampolineHelper} resultStruct The trampoline helper.
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 * TODO bl too long
 */
r5js.ProcCall.prototype.tryIdShim = function(
    continuation, resultStruct, parserProvider) {
  var ans;

  var arg = this.firstOperand;

  /* todo bl: id shims have become quite popular for passing through
     disparate objects on the trampoline. The logic could be made clearer. */
  if (arg instanceof r5js.Macro)
    ans = arg;
  else if (r5js.PrimitiveProcedure.isImplementedBy(arg) ||
      arg instanceof r5js.ast.Lambda)
    ans = arg;
  else if (arg instanceof r5js.ast.Identifier)
    ans = this.env.get(/** @type {string} */ (arg.getPayload()));
  else if (arg instanceof r5js.ast.Quote) {
    var env = this.env;
    // Do the appropriate substitutions.
    ans = arg.replaceChildren(
        function(node) {
          return node instanceof r5js.ast.Identifier && node.shouldUnquote();
        },
        function(node) {
          var ans = r5js.datumutil.maybeWrapResult(env.get(
              /** @type {string} */ ((/** @type {!r5js.ast.Identifier} */ (node)).
                  getPayload())));
            // TODO bl document why we're doing this
            if (ans instanceof r5js.Ref) {
                ans = ans.deref();
            }
          if (node instanceof r5js.ast.Identifier &&
              node.shouldUnquoteSplice()) {
            if (ans instanceof r5js.ast.List) {
              if (ans.getFirstChild()) { // `(1 ,@(list 2 3) 4) => (1 2 3 4)
                ans = ans.getFirstChild();
	      } else { // `(1 ,@(list) 2) => (1 2)
                ans = null;
	      }
            } else throw new r5js.QuasiquoteError(ans + ' is not a list');
          }
          return /** @type {r5js.Datum} */ (ans);
        });
    // Now strip away the quote mark.
    // the newIdOrLiteral part is for (quote quote)
    ans = (ans instanceof r5js.ast.CompoundDatum &&
        ans.getFirstChild()) ?
        ans.getFirstChild() :
        new r5js.ast.Identifier(r5js.parse.Terminals.QUOTE);
  }
  else if (arg instanceof r5js.ast.Quasiquote) {
    resultStruct.nextContinuable = arg.processQuasiquote(
        /** @type {!r5js.IEnvironment} */ (this.env),
        continuation.lastResultName,
        parserProvider
        ).appendContinuable(continuation.nextContinuable);
    return;
  } else if (arg.isImproperList()) {
    throw new r5js.GeneralSyntaxError(arg);
  } else if (arg instanceof r5js.ast.List) {
      ans = arg;
  } else if (arg instanceof r5js.ast.String) {
      ans = arg;
  } else {
    ans = r5js.datumutil.maybeWrapResult(arg.getPayload());
    if (arg.isImmutable()) {
      ans.setImmutable();
    }
  }

  this.bindResult(continuation, ans);

  /* If we're at the end of the continuable-continuation chain and we're
     trying to return a macro object off the trampoline, that's an error.
     The input was a bare macro name. */
  if (!continuation.nextContinuable && ans instanceof r5js.Macro)
    throw new r5js.MacroError(
        /** @type {string} */ (this.firstOperand.getPayload()),
        'bad macro syntax');

  resultStruct.ans = ans;
  resultStruct.nextContinuable = continuation.nextContinuable;
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
      finalArgs.appendSibling(new r5js.ast.Identifier(/** @type {string} */ (arg.getName())));
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
      r5js.procs.newProcCall(
      this.operatorName,
      finalArgs.toSiblings(),
      new r5js.Continuation()));

  var ans = newCallChain.toContinuable();
  ans.setStartingEnv(/** @type {!r5js.IEnvironment} */ (this.env));
  var lastContinuable = ans.getLastContinuable();
  lastContinuable.continuation = continuation;
  resultStruct.nextContinuable = ans;
};


/**
 * Things like set! are represented as ProcCalls whose operators are
 * the small integers in {@link r5js.ProcCall.specialOps}
 * rather than Datum objects.
 * @return {boolean}
 */
r5js.ProcCall.prototype.isSpecialOperator = function() {
  return !(this.operatorName instanceof r5js.Datum);
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

  var specialOp = this.isSpecialOperator();
  var proc = specialOp ?
      this.operatorName :
      this.env.getProcedure(/** @type {string} */ (
          this.operatorName.getPayload()));
  var args = [proc, continuation, resultStruct, parserProvider];

  if (specialOp) {
    this.specialOps.logic[args.shift()].apply(this, args);
  } else if (r5js.PrimitiveProcedure.isImplementedBy(proc)) {
    this.tryPrimitiveProcedure.apply(this, args);
  } else if (proc instanceof r5js.Procedure) {
    this.tryNonPrimitiveProcedure.apply(this, args);
  } else if (proc instanceof r5js.Macro) {
    this.tryMacroUse.apply(this, args);
  } else if (proc instanceof r5js.Continuation) {
    this.tryContinuation.apply(this, args);
  } else if (proc instanceof r5js.JsObjOrMethod) {
    this.tryFFI.apply(this, args);
  } else {
    throw new r5js.EvalError(
        'procedure application: expected procedure, given ' +
            this.operatorName);
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
 * @param {!r5js.Continuation} continuation
 * @param {!r5js.TrampolineHelper} resultStruct
 */
r5js.ProcCall.prototype.tryAssignment = function(continuation, resultStruct) {
  var src = this.env.get(/** @type {string} */ (
      this.firstOperand.getNextSibling().getPayload()));
  /* In Scheme, macros can be bound to identifiers but they are not really
     first-class citizens; you cannot say

     (define x let)

     because the text "let" does not parse as an expression
     (at least if it has its normal binding). In this implementation, however,
     SchemeMacros are objects that go into and come out of Environments
     like any other kind of objects. All kinds of assignments -- top-level,
     internal, syntax, non-syntax -- go through this function, so we
     have to make sure we don't accidentally permit some illegal behavior.

     If we're trying to assign a SchemeMacro object but the isSyntaxAssignment
     flag on the ProcCall object hasn't been set, then the programmer is
     requesting this assignment and we ought to signal an error.

     The situation is complicated a bit because internally, we use let and
     letrec to implement let-syntax and letrec-syntax. In other words,
     we as the implementer do exactly what we forbid the programmer to do.
     We tell the difference between the two parties via the isLetOrLetrecSyntax
     flag on the SchemeMacro object, which only the implementation can set. */
  if (src instanceof r5js.Macro &&
      !src.isLetOrLetrecSyntax() &&
      !this.isSyntaxAssignment) {
    throw new r5js.GeneralSyntaxError(this);
  }
  this.env.mutate(/** @type {string} */ (
      this.firstOperand.getPayload()), src, this.isTopLevelAssignment);
  /* The return value of an assignment is unspecified,
     but this is not the same as no binding. */
  this.bindResult(continuation, r5js.runtime.UNSPECIFIED_VALUE);
  resultStruct.nextContinuable = continuation.nextContinuable;
};


/**
 * Primitive procedure, represented by JavaScript function:
 * (+ x y [ans ...]). We perform the action ("+"), bind the
 * result to the continuation's result name ("ans"), and advance
 * to the next continuable ("...").
 * @param {!r5js.PrimitiveProcedure} proc
 * @param {!r5js.Continuation} continuation
 * @param {!r5js.TrampolineHelper} resultStruct
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 */
r5js.ProcCall.prototype.tryPrimitiveProcedure = function(
    proc, continuation, resultStruct, parserProvider) {

  /* If the operands aren't simple, we'll have to take a detour to
     restructure them. Example:

     (+ (* 1 2) (/ 3 4)) => (* 1 2 [_0 (/ 3 4 [_1 (+ _0 _1 ...)])]) */
  if (!this.operandsInCpsStyle()) {
    this.cpsify(continuation, resultStruct, parserProvider);
  }

  else {
    var args = this.evalArgs(true);
    // todo bl document why we're doing this...
    for (var i = 0; i < args.length; ++i) {
      if (args[i] instanceof r5js.Ref) {
        args[i] = (/** @type {!r5js.Ref} */ (args[i])).deref();
      }
    }
      proc.Call(args, this, continuation, resultStruct);
  }
};


/**
 * Non-primitive procedure, represented by {@link r5js.Procedure} object.
 * Example: suppose we have
 *
 * (define (foo x y) (+ x (* 2 y)))
 *
 * The body of this procedure is desugared as
 *
 * (* 2 y [_0 (+ x _0 [_1 ...])])
 *
 * Then we have the (nested) procedure call
 *
 * (+ 1 (foo 3 4))
 *
 * which is desugared as
 *
 * (foo 3 4 [foo' (+ 1 foo' [_2 ...])])
 *
 * We bind the arguments ("1" and "2") to the formal parameters ("x" and "y"),
 * append the ProcCall's continuation to the end of the Procedure's
 * continuation, and advance to the beginning of the Procedure's body.
 * Thus, on the next iteration of the trampoline loop, we will have
 * the following:
 *
 * (* 2 y [_0 (+ x _0 [foo' (+ 1 foo' [_2 ...])])])
 *
 * @param {!r5js.Procedure} proc The procedure.
 * @param {!r5js.Continuation} continuation A continuation.
 * @param {!r5js.TrampolineHelper} resultStruct The trampoline helper.
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum.
 * @suppress {accessControls} for {@link r5js.Procedure.env_}
 */
r5js.ProcCall.prototype.tryNonPrimitiveProcedure = function(
    proc, continuation, resultStruct, parserProvider) {

  /* If the operands aren't simple, we'll have to take a detour to
     restructure them. */
  if (!this.operandsInCpsStyle()) {
    this.cpsify(continuation, resultStruct, parserProvider);
  }

  else {

    // todo bl we should be able to pass false as the last parameter.
    // need to resolve some bugs.
    var args = this.evalArgs(true);

    /* If we're at a tail call we can reuse the existing environment.
         Otherwise create a new environment pointing back to the current one. */
    var newEnv = proc.isTailCall(continuation) ?
        this.env.allowRedefs() :
        new r5js.Environment(proc.env_).addClosuresFrom(proc.env_);

    /* Remember to discard the new environment
         at the end of the procedure call. */
    if (this.env) {
      continuation.rememberEnv(this.env);
    }

    // Do some bookkeepping to prepare for jumping into the procedure
    proc.setContinuation(continuation);
    proc.checkNumArgs(args.length);
    proc.bindArgs(args, newEnv);
    proc.setEnv(newEnv);

    // And away we go
    resultStruct.nextContinuable = proc.body;
  }
};


/**
 * @param {!r5js.Macro} macro The macro.
 * @param {!r5js.Continuation} continuation A continuation.
 * @param {!r5js.TrampolineHelper} resultStruct The trampoline helper.
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 */
r5js.ProcCall.prototype.tryMacroUse = function(
    macro, continuation, resultStruct, parserProvider) {

  var newEnv = new r5js.Environment(this.env);
  var newParseTree = macro.transcribe(
      this.reconstructDatum(),
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
  // this.reconstructDatum() +
  // ' => ' + newDatumTree);

  var newContinuable = newParseTree.desugar(newEnv, true).
      setStartingEnv(newEnv);

  newContinuable.getLastContinuable().continuation = continuation;
  resultStruct.nextContinuable = newContinuable;
};


/**
 * @param {!r5js.Continuation} proc The continuation.
 * @param {!r5js.Continuation} continuation The following continuation.
 * @param {!r5js.TrampolineHelper} resultStruct The trampoline helper.
 */
r5js.ProcCall.prototype.tryContinuation = function(
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
 * This is my attempt at a JavaScript enum idiom. Is there a better way
 * to get guaranteed constant-time lookup given an ordinal?
 */
r5js.ProcCall.prototype.specialOps = {

  _id: 0,
  _set: 1,

  names: ['id', 'set!'],
  logic: [
    r5js.ProcCall.prototype.tryIdShim,
    r5js.ProcCall.prototype.tryAssignment
  ]
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
