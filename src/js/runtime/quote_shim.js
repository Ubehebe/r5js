goog.provide('r5js.QuoteShim');


goog.require('r5js.ProcCallLike');


/**
 * TODO bl the purpose of this class is unclear.
 * @param {!r5js.ast.Quote} payload
 * @param {string=} opt_continuationName Optional name of the continuation.
 * @extends {r5js.ProcCallLike}
 * @struct
 * @constructor
 */
r5js.QuoteShim = function(payload, opt_continuationName) {
    r5js.QuoteShim.base(this, 'constructor', opt_continuationName);
    /** @const @private */ this.firstOperand_ = payload;
};
goog.inherits(r5js.QuoteShim, r5js.ProcCallLike);

/** @override */
r5js.QuoteShim.prototype.evalAndAdvance = function(resultStruct, env, parserProvider) {
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
r5js.QuoteShim.prototype.tryQuote_ = function(quote) {
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