goog.provide('r5js.newIdShim');


goog.require('r5js.GeneralSyntaxError');
goog.require('r5js.Macro');
goog.require('r5js.MacroError');
goog.require('r5js.ProcCall');
goog.require('r5js.QuasiquoteError');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.Lambda');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Quasiquote');
goog.require('r5js.ast.Quote');
goog.require('r5js.ast.String');



/**
 * @param {?} payload
 * @extends {r5js.ProcCall}
 * @struct
 * @constructor
 */
r5js.IdShim = function(payload) {
  goog.base(this, r5js.IdShim.NAME_, payload);
};
goog.inherits(r5js.IdShim, r5js.ProcCall);


/** @const @private */ r5js.IdShim.NAME_ = new r5js.ast.Identifier('id');


/** @override */
r5js.IdShim.prototype.evalAndAdvance = function(
    continuation, resultStruct, envBuffer, parserProvider) {

  /* If the procedure call has no attached environment, we use
     the environment left over from the previous action on the trampoline. */
  if (!this.env) {
    this.setEnv(/** @type {!r5js.IEnvironment} */ (envBuffer.getEnv()));
  }

  this.tryIdShim_(continuation, resultStruct, parserProvider);

  /* Save the environment we used in case the next action on the trampoline
     needs it (for example branches, which have no environment of their own). */
  envBuffer.setEnv(/** @type {!r5js.IEnvironment} */(this.env));

  // We shouldn't leave the environment pointer hanging around.
  this.clearEnv();
};


/**
 * @param {!r5js.Continuation} continuation A continuation.
 * @param {!r5js.TrampolineHelper} resultStruct The trampoline helper.
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 * @private
 * TODO bl too long.
 */
r5js.ProcCall.prototype.tryIdShim_ = function(
    continuation, resultStruct, parserProvider) {
  var ans;

  var arg = this.firstOperand;

  /* todo bl: id shims have become quite popular for passing through
     disparate objects on the trampoline. The logic could be made clearer. */
  if (arg instanceof r5js.Macro)
    ans = arg;
  else if (r5js.ProcedureLike.isImplementedBy(arg) ||
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
              /** @type {string} */ ((
              /** @type {!r5js.ast.Identifier} */ (node)).
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
    var continuable = /** @type {!r5js.Continuable} */ (arg.processQuasiquote(
        /** @type {!r5js.IEnvironment} */ (this.env),
        continuation.getLastResultName(),
        parserProvider));
    var nextContinuable = continuation.getNextContinuable();
    if (nextContinuable) {
      r5js.ProcCallLike.appendContinuable(
          continuable.getSubtype(), nextContinuable);
    }
    resultStruct.setNextProcCallLike(continuable.getSubtype());
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

  var nextContinuable = continuation.getNextContinuable();

  /* If we're at the end of the continuable-continuation chain and we're
     trying to return a macro object off the trampoline, that's an error.
     The input was a bare macro name. */
  if (!nextContinuable && ans instanceof r5js.Macro)
    throw new r5js.MacroError(
        /** @type {string} */ (this.firstOperand.getPayload()),
        'bad macro syntax');

  resultStruct.setValue(ans);
  if (nextContinuable) {
    resultStruct.setNextProcCallLike(nextContinuable.getSubtype());
  }
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
r5js.newIdShim = function(payload, opt_continuationName) {
  return new r5js.Continuable(
      new r5js.IdShim(payload),
      new r5js.Continuation(opt_continuationName));
};

