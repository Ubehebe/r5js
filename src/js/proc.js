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
goog.provide('r5js.Procedure');
goog.provide('r5js.procs');


goog.require('r5js.Continuable');
goog.require('r5js.ContinuableHelper');
goog.require('r5js.Continuation');
goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.EvalError');
goog.require('r5js.FFIError');
goog.require('r5js.IllegalEmptyApplication');
goog.require('r5js.IncorrectNumArgs');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.JsObjOrMethod');
goog.require('r5js.Macro');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.MacroError');
goog.require('r5js.runtime.PrimitiveProcedure');
goog.require('r5js.QuasiquoteError');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.TooFewArgs');
goog.require('r5js.data');



/**
 * @param {!Array.<string>} formalsArray The procedure's formal parameters,
 *        in order.
 * @param {boolean} isDotted True iff this is a dotted procedure, such as
 *        (define (head x . y) x).
 * @param {?} bodyStart
 * @param {!r5js.IEnvironment} env An environment.
 * @param {string=} opt_name The procedure's name. It has no semantic
 *     importance; it's just used for pretty-printing debugs and messages
 *     to the user. If not given, one will be created.
 * @constructor
 */
r5js.Procedure = function(formalsArray, isDotted, bodyStart, env, opt_name) {
  /**
     * @type {boolean}
     */
  this.isDotted = isDotted;

    /** @const {string} */
    this.name = goog.isDef(opt_name) ? opt_name : ('' + goog.getUid(this));

  /**
     * @type {!r5js.IEnvironment}
     */
  this.env = env.newChildEnv(this.name);

  /**
     * @type {!Array.<string>}
     */
  this.formalsArray = formalsArray;

  if (bodyStart) {

    /* R5RS 5.2.2: "A <body> containing internal definitions can always
        be converted into a completely equivalent letrec expression." */
    var letrecBindings = new r5js.SiblingBuffer();
    for (var cur = bodyStart;
         cur && cur.peekParse() === r5js.parse.Nonterminals.DEFINITION;
         cur = cur.getNextSibling()) {
      cur.forEach(function(node) {
        if (node.getFirstChild() &&
            node.getFirstChild().getPayload() === r5js.parse.Terminals.DEFINE) {
          letrecBindings.appendSibling(node.extractDefinition());
        }
      });
    }

    if (letrecBindings.isEmpty()) {
      this.body = cur.sequence(this.env);
    } else {
      var letrec = newEmptyList();
      letrec.setFirstChild(letrecBindings.toSiblings());
      letrec.setNextSibling(cur);
      this.body = r5js.procs.newProcCall(
          r5js.data.newIdOrLiteral('letrec'),
          letrec,
          new r5js.Continuation());
    }

    this.lastContinuable = this.body.getLastContinuable();
  }
};


/**
 * @param {!r5js.Environment} env Environment to clone with.
 * @return {!r5js.Procedure} A clone of this procedure, with the given
 *         environment.
 */
r5js.Procedure.prototype.cloneWithEnv = function(env) {
  var ans = new r5js.Procedure(this.formalsArray, this.isDotted, null, env);
  ans.env.setClosuresFrom(this.env); // non-cloning ok?
  ans.body = this.body;
  ans.lastContinuable = this.lastContinuable;
  return ans;
};


/**
 * @param {!r5js.Continuation} c A continuation.
 */
r5js.Procedure.prototype.setContinuation = function(c) {
  /* This will be a vacuous write for a tail call. But that is
    probably still faster than checking if we are in tail position and,
    if so, explicitly doing nothing. */
  if (this.lastContinuable) {
    this.lastContinuable.continuation = c;
  }
};


/**
 * @param {!r5js.IEnvironment} env The environment to set.
 */
r5js.Procedure.prototype.setEnv = function(env) {
  /* todo bl is it possible to have a procedure body whose first
     continuable is a branch? hopefully not, and I can remove
     the second check. */
  if (this.body) {
    //        if (this.body.subtype instanceof r5js.ProcCall) {
    if (this.body.subtype.setEnv) {
      this.body.subtype.setEnv(env, true);
    } else {
      throw new r5js.InternalInterpreterError(
          'invariant incorrect -- procedure does not begin with proc call');
    }
  }
};


/**
 * @param {!r5js.Continuation} c A continuation.
 * @return {boolean} True iff this procedure is in tail position.
 * TODO bl are we sure this covers all forms of tail recursion in R5RS?
 */
r5js.Procedure.prototype.isTailCall = function(c) {
  if (this.lastContinuable && this.lastContinuable.continuation === c) {
    // a good place to see if tail recursion is actually working :)
    // console.log('TAIL RECURSION!!!');
    return true;
  } else return false;
};


/** @override */
r5js.Procedure.prototype.toString = function() {
  return 'proc:' + this.name;
};


/**
 * @param {number} numActuals The number of arguments passed to the procedure
 *        during evaluation.
 */
r5js.Procedure.prototype.checkNumArgs = function(numActuals) {

  if (!this.isDotted) {
    if (numActuals !== this.formalsArray.length)
      throw new r5js.IncorrectNumArgs(
          this.toString(), this.formalsArray.length, numActuals);
  } else {
    var minNumArgs = this.formalsArray.length - 1;
    if (numActuals < minNumArgs)
      throw new r5js.TooFewArgs(this.toString(), minNumArgs, numActuals);
  }
};


/**
 * @param {!Array.<*>} args TODO bl narrow.
 * @param {!r5js.IEnvironment} env
 */
r5js.Procedure.prototype.bindArgs = function(args, env) {

  var name, i;

  for (i = 0; i < this.formalsArray.length - 1; ++i) {
    name = this.formalsArray[i];
    env.addBinding(name, args[i]);
  }

  if (this.formalsArray.length > 0) {

    name = this.formalsArray[i];
    if (!this.isDotted) {
      env.addBinding(name, args[i]);
    } else {
      // Roll up the remaining arguments into a list
      var list = newEmptyList();
      // Go backwards and do prepends to avoid quadratic performance
      for (var j = args.length - 1; j >= this.formalsArray.length - 1; --j)
        list.prependChild(args[j]);
      env.addBinding(name, list);
    }
  }
};


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
        .appendSibling(r5js.data.newIdOrLiteral(dstName))
        .appendSibling(r5js.data.newIdOrLiteral(srcName))
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
 * @param {string} dstName
 * @param {string} srcName
 * @param {!r5js.Continuation} continuation
 * @return {!r5js.Continuable}
 */
function newAssignment(dstName, srcName, continuation) {
  var operands = new r5js.SiblingBuffer()
        .appendSibling(r5js.data.newIdOrLiteral(dstName))
        .appendSibling(r5js.data.newIdOrLiteral(srcName))
        .toSiblings();

  return r5js.procs.newProcCall(
      r5js.ProcCall.prototype.specialOps._set,
      operands,
      continuation
  );
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
  ans += '(' + (this.operatorName instanceof r5js.Datum ?
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
  var op = r5js.data.newIdOrLiteral(this.operatorName.getPayload());
  op.setNextSibling(this.firstOperand);
  var ans = newEmptyList();
  ans.appendChild(op);
  return ans;
};


/**
 * @return {boolean} True iff the operands are in continuation-passing style.
 */
r5js.ProcCall.prototype.operandsInCpsStyle = function() {
  for (var cur = this.firstOperand; cur; cur = cur.nextSibling_) {
    if (cur instanceof r5js.Datum) {
      if (cur.isEmptyList()) {
        throw new r5js.IllegalEmptyApplication(this.operatorName.getPayload());
      } else if (!cur.isLiteral()) {
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
 */
r5js.ProcCall.prototype.tryIdShim = function(
    continuation, resultStruct, parserProvider) {
  var ans;

  var arg = this.firstOperand;

  /* todo bl: id shims have become quite popular for passing through
     disparate objects on the trampoline. The logic could be made clearer. */
  if (arg instanceof r5js.Macro)
    ans = arg;
  else if (r5js.runtime.PrimitiveProcedure.isImplementedBy(arg) ||
      arg.isProcedure())
    ans = arg;
  else if (arg.isIdentifier())
    ans = this.env.get(arg.getPayload());
  else if (arg.isQuote()) {
    var env = this.env;
    // Do the appropriate substitutions.
    ans = arg.replaceChildren(
        function(node) {
          return node.shouldUnquote();
        },
        function(node) {
          var ans = r5js.data.maybeWrapResult(env.get(node.getPayload())).
              maybeDeref();
          if (node.shouldUnquoteSplice()) {
            if (ans.isList()) {
              if (ans.getFirstChild()) { // `(1 ,@(list 2 3) 4) => (1 2 3 4)
                ans = ans.getFirstChild();
	      } else { // `(1 ,@(list) 2) => (1 2)
                ans = null;
	      }
            } else throw new r5js.QuasiquoteError(ans + ' is not a list');
          }
          return ans;
        });
    // Now strip away the quote mark.
    // the newIdOrLiteral part is for (quote quote)
    ans = ans.getFirstChild() ?
        ans.getFirstChild() :
        r5js.data.newIdOrLiteral(r5js.parse.Terminals.QUOTE);
  }
  else if (arg.isQuasiquote()) {
    resultStruct.nextContinuable = processQuasiquote(
        arg,
        /** @type {!r5js.IEnvironment} */ (this.env),
        continuation.lastResultName,
        parserProvider
        ).appendContinuable(continuation.nextContinuable);
    return;
  } else if (arg.isImproperList()) {
    throw new r5js.GeneralSyntaxError(arg);
  } else {
    ans = r5js.data.maybeWrapResult(arg.getPayload(), arg.getType());
    if (arg.isImmutable())
      ans.setImmutable();
  }

  this.bindResult(continuation, ans);

  /* If we're at the end of the continuable-continuation chain and we're
     trying to return a macro object off the trampoline, that's an error.
     The input was a bare macro name. */
  if (!continuation.nextContinuable && ans instanceof r5js.Macro)
    throw new r5js.MacroError(this.firstOperand, 'bad macro syntax');

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
    if (arg.isQuote())
      finalArgs.appendSibling(arg.clone().normalizeInput());
    else if (arg.isQuasiquote()) {
      if ((maybeContinuable =
          processQuasiquote(
          arg,
          /** @type {!r5js.IEnvironment} */ (this.env),
          continuation.lastResultName,
          parserProvider)) instanceof r5js.Continuable) {
        finalArgs.appendSibling(
            r5js.data.newIdOrLiteral(maybeContinuable
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
        arg.setType("'");
        finalArgs.appendSibling(arg);
      }
    } else if (arg.isProcedure()) {
      finalArgs.appendSibling(r5js.data.newIdOrLiteral(arg.getName()));
    } else if (arg.isImproperList()) {
      throw new r5js.GeneralSyntaxError(arg);
    } else if ((maybeContinuable = arg.desugar(this.env)) instanceof
        r5js.Continuable) {
      /* todo bl is it an invariant violation to be a list
             and not to desugar to a Continuable? */
      finalArgs.appendSibling(
          r5js.data.newIdOrLiteral(
              maybeContinuable.
                  getLastContinuable().continuation.lastResultName));
      newCallChain.appendContinuable(maybeContinuable);
    } else {
      finalArgs.appendSibling(r5js.data.newIdOrLiteral(arg.getPayload(), arg.getType()));
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
      this.env.getProcedure(this.operatorName.getPayload());
  var args = [proc, continuation, resultStruct, parserProvider];

  if (specialOp) {
    this.specialOps.logic[args.shift()].apply(this, args);
  } else if (r5js.runtime.PrimitiveProcedure.isImplementedBy(proc)) {
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
 * @param {*} val TODO bl.
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
  var src = this.env.get(this.firstOperand.getNextSibling().getPayload());
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
      !src.isLetOrLetrecSyntax &&
      !this.isSyntaxAssignment) {
    throw new r5js.GeneralSyntaxError(this);
  }
  this.env.mutate(this.firstOperand.getPayload(), src, this.isTopLevelAssignment);
  /* The return value of an assignment is unspecified,
     but this is not the same as no binding. */
  this.bindResult(continuation, null);
  resultStruct.nextContinuable = continuation.nextContinuable;
};


/**
 * Primitive procedure, represented by JavaScript function:
 * (+ x y [ans ...]). We perform the action ("+"), bind the
 * result to the continuation's result name ("ans"), and advance
 * to the next continuable ("...").
 * @param {!r5js.runtime.PrimitiveProcedure} proc
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
      if (args[i] instanceof r5js.Datum)
        args[i] = args[i].maybeDeref();
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
        new r5js.Environment(null /* name */, proc.env).
            addClosuresFrom(proc.env);

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

  var newEnv = new r5js.Environment(null /* name */, this.env);
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
 * @return {?}
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
  if (this.firstOperand instanceof r5js.Datum &&
      !this.firstOperand.getNextSibling() &&
      this.firstOperand.isIdentifier()) {
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
    } else if (cur.isIdentifier()) {
      var toPush = wrapArgs ?
          r5js.data.maybeWrapResult(this.env.get(cur.getPayload())) :
          this.env.get(cur.getPayload());
      /* Macros are not first-class citizens in Scheme; they cannot
             be passed as arguments. Internally, however, we do just that
             for convenience. The isLetOrLetrecSyntax flag discriminates
             between the programmer and the implementation. */
      if (toPush instanceof r5js.Macro &&
          !toPush.isLetOrLetrecSyntax) {
        throw new r5js.MacroError(cur.getPayload(), 'bad syntax');
      }
      args.push(toPush);
    }
    else if (cur.isQuote()) {
      cur.normalizeInput();
      // the newIdOrLiteral part is for (quote quote)
      args.push(cur.getFirstChild() ?
          cur.getFirstChild() :
          r5js.data.newIdOrLiteral(r5js.parse.Terminals.QUOTE));
    }
    else if (cur.isProcedure()) {
      args.push(cur);
    } else if (cur.getPayload() !== undefined) {
      args.push(r5js.data.maybeWrapResult(cur.getPayload(), cur.getType()));
    }
    else throw new r5js.InternalInterpreterError('unexpected datum ' + cur);
  }

  return args;
};


/**
 * Example: `(1 ,(+ 2 3)) should desugar as (+ 2 3 [_0 (id (1 _0) [_2 ...])])
 * Note: this was once an instance method on {@link r5js.Datum}, defined
 * in datum.js. I moved it here (its only point of use) to break
 * a cyclic dependency between Datum and the parser, and thought it was
 * cleaner at that point to turn it into a regular function.
 * @param {!r5js.Datum} datum Datum to process.
 * @param {!r5js.IEnvironment} env TODO bl.
 * @param {string} cpsName TODO bl.
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 * @return {*} TODO bl.
 * @suppress {const} for the assignment to continuation.lastResultName,
 * which may indicate a bug. TODO bl investigate.
 */
function processQuasiquote(datum, env, cpsName, parserProvider) {

  var newCalls = new r5js.ContinuableHelper();

  var qqLevel = datum.getQQLevel();

  datum.replaceChildren(
      function(node) {
        return node.isUnquote() && (node.getQQLevel() === qqLevel);
      },
      function(node) {
        var asContinuable = /** @type {!r5js.Continuable} */ (parserProvider(
            /** @type {!r5js.Datum} */(node.getFirstChild())).
                parse(r5js.parse.Nonterminals.EXPRESSION).
                desugar(env, true));
        var continuation = asContinuable.getLastContinuable().continuation;
        /* Throw out the last result name and replace it with another
             identifier (also illegal in Scheme) that will let us know if it's
             unquotation or unquotation with splicing. */
        continuation.lastResultName = node.getType() + '' + goog.getUid(new Object());
        newCalls.appendContinuable(asContinuable);
        return r5js.data.newIdOrLiteral(continuation.lastResultName);
      });

  datum.setType(r5js.DatumType.QUOTE);

  newCalls.appendContinuable(newIdShim(datum, cpsName));
  var ans = newCalls.toContinuable();
  return ans && ans.setStartingEnv(env);
}

