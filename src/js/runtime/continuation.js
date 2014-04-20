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


goog.provide('r5js.Continuation');


goog.require('r5js.ProcedureLike');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Macro');
// TODO bl cyclic dependency goog.require('r5js.newProcCall');



/**
 * Example: (g (f x y) z) desugared is
 *
 * (f x y [f' (g f' z [g' ...])])
 *
 * The continuation c is [f' (g f' z [g' ...])]
 *
 * c.lastResultName is f'
 * c.nextContinuable is (g f' z ...)
 *
 *
 * @param {string=} opt_lastResultName Optional name to use for the last result.
 *     If not given, a unique name will be created.
 *     @implements {r5js.ProcedureLike}
 *     @struct
 * @constructor
 */
r5js.Continuation = function(opt_lastResultName) {
  /** @private {string} */
  this.lastResultName_ = goog.isDef(opt_lastResultName) ?
      opt_lastResultName :
      ('@' /* TODO bl document */ + goog.getUid(this));

  /** @private {r5js.ProcCallLike} */ this.nextContinuable_ = null;
  /** @private {r5js.ProcCallLike} */ this.beforeThunk_ = null;
};
r5js.ProcedureLike.addImplementation(r5js.Continuation);


/** @return {string} */
r5js.Continuation.prototype.getLastResultName = function() {
  return this.lastResultName_;
};


/**
 * @param {string} name
 * TODO bl This setter doesn't make sense. lastResultName should be const.
 */
r5js.Continuation.prototype.setLastResultName = function(name) {
  this.lastResultName_ = name;
};


/** @return {r5js.ProcCallLike} */
r5js.Continuation.prototype.getNextContinuable = function() {
  return this.nextContinuable_;
};


/** @param {!r5js.ProcCallLike} continuable */
r5js.Continuation.prototype.setNextContinuable = function(continuable) {
  this.nextContinuable_ = continuable;
};


/**
 * Just for call/ccs inside dynamic-winds.
 * TODO bl: document why we don't have to install the "after" thunk.
 * (I'm pretty sure the reason is it's already in the continuable chain
 * somewhere.)
 * @param {!r5js.ProcCallLike} before
 */
r5js.Continuation.prototype.installBeforeThunk = function(before) {
  this.beforeThunk_ = before;
};


/**
 * @return {?}
 * TODO bl: previously, this did an instanceof check on
 * this.nextContinuable.subtype and returned null if it wasn't
 * a {@link r5js.ProcCall} (in particular, if it was a {@link r5js.Branch_}).
 * The instanceof check caused an indirect circular dependency between
 * {@link r5js.Continuation} and {@link r5js.ProcCall}. This method
 * was the easiest way to break the cycle, as it had only one caller:
 * {@link r5js.ProcCall.bindResult}. So I moved the instanceof check
 * to the call site.
 */
r5js.Continuation.prototype.getAdjacentProcCall = function() {
  return this.nextContinuable_;
};


/** @param {!r5js.IEnvironment} env Environment to remember. */
r5js.Continuation.prototype.rememberEnv = function(env) {
  /* In general, we need to remember to jump out of the newEnv at
     the end of the procedure body. See ProcCall.prototype.maybeSetEnv
     for detailed logic (and maybe bugs). */
  if (this.nextContinuable_) {
    this.nextContinuable_.maybeSetEnv(env);
  }
};


/**
 * @override
 * @suppress {accessControls}
 */
r5js.Continuation.prototype.evalAndAdvance = function(
    procCall, continuation, trampolineHelper, parserProvider) {
  var arg = procCall.evalArgs(false)[0]; // there will only be 1 arg
  procCall.env.addBinding(this.lastResultName_, arg);
  trampolineHelper.setValue(arg);
  if (this.nextContinuable_) {
    trampolineHelper.setNextProcCallLike(this.nextContinuable_);
  }

  if (this.beforeThunk_) {
    var before = this.beforeThunk_;
    var cur = this.nextContinuable_;
    if (cur) {
      r5js.ProcCallLike.appendProcCallLike(before, cur);
    }
    trampolineHelper.setNextProcCallLike(before);
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
  for (var tmp = trampolineHelper.getNextProcCallLike(), prev;
      tmp;
      prev = tmp, tmp = tmp.getContinuation().nextContinuable_) {
    if (tmp === procCall) {
      if (prev) {
        prev.getContinuation().nextContinuable_ =
            tmp.getContinuation().nextContinuable_;
      }
      break;
    }
  }
};


/**
 * R5RS 4.3.1: "Let-syntax and letrec-syntax are analogous to let and letrec,
 * but they bind syntactic keywords to macro transformers instead of binding
 * variables to locations that contain values."
 *
 * In this implementation, a macro is just another kind of object that can
 * be stored in an environment, so we reuse the existing let machinery.
 * For example:
 *
 * (let-syntax ((foo (syntax-rules () ((foo) 'hi)))) ...)
 *
 * desugars as
 *
 * (let ((foo [SchemeMacro object])) ...)
 *
 * We just need to be sure that the SchemeMacro object inserted directly
 * into the parse tree plays well when the tree is transcribed and reparsed.
 *
 * @param {!r5js.ast.CompoundDatum} datum Datum to desugar.
 * @param {!r5js.IEnvironment} env TODO bl.
 * @param {string} operatorName TODO bl.
 * @return {!r5js.ProcCallLike}
 */
r5js.Continuation.desugarMacroBlock = function(datum, env, operatorName) {

  var letBindings = new r5js.SiblingBuffer();

  datum.firstSublist().forEachChild(function(spec) {
    spec = /** @type {!r5js.ast.CompoundDatum} */ (spec); // TODO bl
    var kw = spec.at(r5js.parse.Nonterminals.KEYWORD).clone(null /* parent */);
    var macro = /** @type {!r5js.Macro} */ (
        spec.at(r5js.parse.Nonterminals.TRANSFORMER_SPEC).desugar(env));
    var buf = new r5js.SiblingBuffer();
    /* We have to wrap the SchemeMacro object in a Datum to get it into
         the parse tree. */
    buf.appendSibling(kw);
    buf.appendSibling(new r5js.ast.Macro(macro));
    letBindings.appendSibling(buf.toList(r5js.ast.List));
  });

  var _let = new r5js.SiblingBuffer();
  _let.appendSibling(
      letBindings.toList(r5js.ast.List)
  ).appendSibling(
      /** @type {!r5js.Datum} */ (datum.firstSublist().getNextSibling()));

  return r5js.newProcCall(
      new r5js.ast.Identifier(operatorName), _let.toSiblings());
};
