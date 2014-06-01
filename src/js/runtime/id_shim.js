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

goog.provide('r5js.IdShim');


goog.require('r5js.GeneralSyntaxError');
goog.require('r5js.Macro');
goog.require('r5js.MacroError');
goog.require('r5js.ProcCallLike');
goog.require('r5js.QuasiquoteError');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Quasiquote');
goog.require('r5js.ast.Quote');



/**
 * If a nonterminal in the grammar has no associated desugar function,
 * desugaring it will be a no-op. That is often the right behavior,
 * but sometimes we would like to wrap the datum in a Continuable
 * object for convenience on the trampoline. For example, the program
 * "1 (+ 2 3)" should be desugared as (id 1 [_0 (+ 2 3 [_1 ...])]).
 *
 * We represent these id shims as ProcCalls whose operatorNames are null
 * and whose firstOperand is the payload.
 * @param {r5js.Datum} payload
 * @param {string=} opt_continuationName Optional name of the continuation.
 * @extends {r5js.ProcCallLike}
 * @struct
 * @constructor
 */
r5js.IdShim = function(payload, opt_continuationName) {
  goog.base(this, opt_continuationName);
  /** @const @private */ this.firstOperand_ = payload;
};
goog.inherits(r5js.IdShim, r5js.ProcCallLike);


/** @override */
r5js.IdShim.prototype.evalAndAdvance = function(
    resultStruct, env, parserProvider) {
  var ans;
  if (this.firstOperand_ instanceof r5js.ast.Identifier) {
    ans = this.tryIdentifier_(this.firstOperand_);
  } else if (this.firstOperand_ instanceof r5js.ast.Quote) {
    ans = this.tryQuote_(this.firstOperand_);
  } else if (this.firstOperand_ instanceof r5js.ast.Quasiquote) {
    var next = this.tryQuasiquote_(this.firstOperand_, parserProvider);
    if (next) {
      resultStruct.setNext(next);
    }
    return; // TODO bl odd control flow
  } else if (this.firstOperand_.isImproperList()) {
    throw new r5js.GeneralSyntaxError(this.firstOperand_);
  } else {
    ans = this.firstOperand_;
  }

  if (ans !== null) {
    this.bindResult(ans);
    resultStruct.setValue(ans);
  }

  var nextContinuable = this.getNext();

  /* If we're at the end of the continuable-continuation chain and we're
     trying to return a macro object off the trampoline, that's an error.
     The input was a bare macro name. */
  if (!nextContinuable && ans instanceof r5js.Macro) {
    throw new r5js.MacroError('TODO bl', 'bad macro syntax');
  }

  if (nextContinuable) {
    resultStruct.setNext(nextContinuable);
  }
};


/**
 * @param {!r5js.ast.Identifier} id
 * @return {?r5js.runtime.Value}
 * @private
 */
r5js.IdShim.prototype.tryIdentifier_ = function(id) {
  return this.getEnv().get(/** @type {string} */ (id.getPayload()));
};


/**
 * @param {!r5js.ast.Quote} quote
 * @return {?r5js.runtime.Value}
 * @private
 */
r5js.IdShim.prototype.tryQuote_ = function(quote) {
  var env = this.getEnv();
  // Do the appropriate substitutions.
  var ans = quote.replaceChildren(
      function(node) {
        return node instanceof r5js.ast.Identifier && node.shouldUnquote();
      },
      function(node) {
        var result = env.get(
            /** @type {string} */ ((
            /** @type {!r5js.ast.Identifier} */ (node)).
            getPayload()));
        var ans = result === null ?
            r5js.runtime.UNSPECIFIED_VALUE :
            r5js.datumutil.wrapValue(result);
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
  return (ans instanceof r5js.ast.CompoundDatum &&
      ans.getFirstChild()) ?
      ans.getFirstChild() :
      new r5js.ast.Identifier(r5js.parse.Terminals.QUOTE);
};


/**
 * @param {!r5js.ast.Quasiquote} quasiquote
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider
 * @return {r5js.ProcCallLike}
 * @private
 */
r5js.IdShim.prototype.tryQuasiquote_ = function(quasiquote, parserProvider) {
  var continuable = quasiquote.processQuasiquote(
      /** @type {!r5js.IEnvironment} */ (this.getEnv()),
      this.getResultName(),
      parserProvider);
  var next = this.getNext();
  if (next) {
    r5js.ProcCallLike.appendProcCallLike(continuable, next);
  }
  return continuable;
};
