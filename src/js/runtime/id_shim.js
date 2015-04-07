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

goog.provide('r5js.idShim');


goog.require('r5js.ProcCallLike');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Quasiquote');
goog.require('r5js.ast.Quote');
goog.require('r5js.error');
goog.require('r5js.QuasiquoteShim');
goog.require('r5js.runtime.UNSPECIFIED_VALUE');

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
 * @private
 */
r5js.IdShim_ = function(payload, opt_continuationName) {
  r5js.IdShim_.base(this, 'constructor', opt_continuationName);
  /** @const @private */ this.firstOperand_ = payload;
};
goog.inherits(r5js.IdShim_, r5js.ProcCallLike);


/** @override */
r5js.IdShim_.prototype.evalAndAdvance = function(
    resultStruct, env, parserProvider) {
    const ans = this.firstOperand_ instanceof r5js.ast.Identifier
        ? this.tryIdentifier_(this.firstOperand_)
        : this.firstOperand_;

    if (ans !== null) {
        this.bindResult(ans);
        resultStruct.setValue(ans);
    }

    const nextContinuable = this.getNext();

    if (nextContinuable) {
        resultStruct.setNext(nextContinuable);
    }
};


/**
 * @param {!r5js.ast.Identifier} id
 * @return {?r5js.runtime.Value}
 * @private
 */
r5js.IdShim_.prototype.tryIdentifier_ = function(id) {
  return this.getEnv().get(/** @type {string} */ (id.getPayload()));
};


/**
 * TODO bl the purpose of this class is unclear.
 * @param {!r5js.ast.Quote} payload
 * @param {string=} opt_continuationName Optional name of the continuation.
 * @extends {r5js.ProcCallLike}
 * @struct
 * @constructor
 * @private
 */
r5js.QuoteShim_ = function(payload, opt_continuationName) {
    r5js.QuoteShim_.base(this, 'constructor', opt_continuationName);
    /** @const @private */ this.firstOperand_ = payload;
};
goog.inherits(r5js.QuoteShim_, r5js.ProcCallLike);

/** @override */
r5js.QuoteShim_.prototype.evalAndAdvance = function(resultStruct, env, parserProvider) {
    const ans = this.tryQuote_(this.firstOperand_);
    if (ans !== null) {
        this.bindResult(ans);
        resultStruct.setValue(ans);
    }

    const nextContinuable = this.getNext();

    if (nextContinuable) {
        resultStruct.setNext(nextContinuable);
    }
};

/**
 * @param {!r5js.ast.Quote} quote
 * @return {?r5js.runtime.Value}
 * @private
 */
r5js.QuoteShim_.prototype.tryQuote_ = function(quote) {
    const env = this.getEnv();
    // Do the appropriate substitutions.
    const ans = quote.replaceChildren(
        function(node) {
            return node instanceof r5js.ast.Identifier && node.shouldUnquote();
        },
        function(node) {
            const result = env.get(
                /** @type {string} */ ((
                /** @type {!r5js.ast.Identifier} */ (node)).
                    getPayload()));
            let ans = result === null ?
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
                } else throw r5js.error.quasiquote(ans + ' is not a list');
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
 * @param {r5js.Datum} payload
 * @param {string=} opt_continuationName Optional name of the continuation.
 * @return {!r5js.ProcCallLike}
 */
r5js.idShim = function(payload, opt_continuationName) {
    if (payload instanceof r5js.ast.Quasiquote) {
        return new r5js.QuasiquoteShim(payload, opt_continuationName);
    } else if (payload instanceof r5js.ast.Quote) {
        return new r5js.QuoteShim_(payload, opt_continuationName);
    } else {
        return new r5js.IdShim_(payload, opt_continuationName);
    }
};
