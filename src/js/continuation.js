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


goog.require('r5js.Branch');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.MacroDatum');
goog.require('r5js.ast.Identifier');
// TODO bl cyclic dependency goog.require('r5js.procs');



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
 * @constructor
 * @suppress {const} See {@link r5js.Quasiquote#processQuasiquote}.
 */
r5js.Continuation = function(opt_lastResultName) {
  /** @const {string} */
  this.lastResultName = goog.isDef(opt_lastResultName) ?
      opt_lastResultName :
      ('@' /* TODO bl document */ + goog.getUid(this));
};


/**
 * Just for call/ccs inside dynamic-winds.
 * TODO bl: document why we don't have to install the "after" thunk.
 * (I'm pretty sure the reason is it's already in the continuable chain
 * somewhere.)
 * @param {?} before
 */
r5js.Continuation.prototype.installBeforeThunk = function(before) {
  this.beforeThunk = before;
};


/**
 * Just for debugging.
 * @param {?number} indentLevel Indentation level for output.
 * @return {string} A textual representation of the continuation.
 */
r5js.Continuation.prototype.debugString = function(indentLevel) {

  if (indentLevel == null) {
    /* If no indent level is given, this function is being used to
         construct an external representation, so we should hide all the
         implementation details. It's legal to return continuations directly,
         as in

         (define x 3)
         (call-with-current-continuation (lambda (c) (set! x c)))
         x
         */
    return '[continuation]';
  } else {

    // Otherwise this is being used for debugging, show all the things.

    var ans = '[' + this.lastResultName;

    if (this.nextContinuable) {
      for (var i = 0; i < indentLevel; ++i)
        ans += '\t';
      ans += ' ' + this.nextContinuable.debugString(indentLevel + 1);
    }
    return ans + ']';
  }
};


/**
 * @return {?}
 * TODO bl: previously, this did an instanceof check on
 * this.nextContinuable.subtype and returned null if it wasn't
 * a {@link r5js.ProcCall} (in particular, if it was a {@link r5js.Branch}).
 * The instanceof check caused an indirect circular dependency between
 * {@link r5js.Continuation} and {@link r5js.ProcCall}. This method
 * was the easiest way to break the cycle, as it had only one caller:
 * {@link r5js.ProcCall.bindResult}. So I moved the instanceof check
 * to the call site.
 */
r5js.Continuation.prototype.getAdjacentProcCall = function() {
  return this.nextContinuable && this.nextContinuable.subtype;
};


/**
 * @param {!r5js.IEnvironment} env Environment to remember.
 */
r5js.Continuation.prototype.rememberEnv = function(env) {
  /* In general, we need to remember to jump out of the newEnv at
     the end of the procedure body. See ProcCall.prototype.maybeSetEnv
     for detailed logic (and maybe bugs). */
  if (this.nextContinuable) {
    var next = this.nextContinuable.subtype;
    if (next instanceof r5js.ProcCall)
      next.maybeSetEnv(env);
  /* Somewhat tricky. We can't know in advance which branch we'll take,
         so we set the environment on both branches. Later, when we actually
         decide which branch to take, we must clear the environment on the
         non-taken branch to prevent old environments from hanging around.

         todo bl: it would probably be better to remember the environment on
         the Branch directly. Then Branch.prototype.evalAndAdvance can set the
         environment on the taken branch without having to remember to clear
         it off the non-taken branch. I'll save this for the next time
         I refactor ProcCalls and Branches. (The explicit "subtypes" suggest
         my command of prototypal inheritance wasn't great when I wrote
         this code.) */
    else if (next instanceof r5js.Branch) {
      if (next.consequent.subtype instanceof r5js.ProcCall) {
        next.consequent.subtype.maybeSetEnv(env);
      }
      if (next.alternate.subtype instanceof r5js.ProcCall) {
        next.alternate.subtype.maybeSetEnv(env);
      }
    } else throw new r5js.InternalInterpreterError('invariant incorrect');
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
 * @param {!r5js.Datum} datum Datum to desugar.
 * @param {!r5js.IEnvironment} env TODO bl.
 * @param {string} operatorName TODO bl.
 * @return {!r5js.Continuable}
 */
r5js.Continuation.desugarMacroBlock = function(datum, env, operatorName) {

  var letBindings = new r5js.SiblingBuffer();

    datum.at(r5js.parse.Terminals.LPAREN).forEachChild(function(spec) {
    var kw = spec.at('keyword').clone(null /* parent */);
    var macro = /** @type {!r5js.Macro} */ (
        spec.at('transformer-spec').desugar(env));
    var buf = new r5js.SiblingBuffer();
    /* We have to wrap the SchemeMacro object in a Datum to get it into
         the parse tree. */
    buf.appendSibling(kw);
    buf.appendSibling(new r5js.MacroDatum(macro));
    letBindings.appendSibling(buf.toList());
  });

  var _let = new r5js.SiblingBuffer();
  _let.appendSibling(letBindings.toList());
  _let.appendSibling(/** @type {!r5js.Datum} */ (datum.at('(').getNextSibling()));

  return r5js.procs.newProcCall(
      new r5js.ast.Identifier(operatorName),
      _let.toSiblings(),
      new r5js.Continuation());
};
