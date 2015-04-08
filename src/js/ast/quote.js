goog.provide('r5js.ast.Quote');

goog.require('r5js.IPair');
goog.require('r5js.ast.CompoundDatum');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.List');
goog.require('r5js.parse.Terminals');
goog.require('r5js.ProcCallLike');

/**
 * @param {r5js.Datum} firstChild
 * @implements {r5js.IPair}
 * @extends {r5js.ast.CompoundDatum}
 * @struct
 * @constructor
 */
r5js.ast.Quote = function(firstChild) {
  r5js.ast.Quote.base(this, 'constructor');
  if (firstChild) {
    this.setFirstChild(firstChild.setImmutable());
  }
};
goog.inherits(r5js.ast.Quote, r5js.ast.CompoundDatum);
r5js.IPair.addImplementation(r5js.ast.Quote);


/** @const @private {!r5js.runtime.Value} */
r5js.ast.Quote.CAR_ = new r5js.ast.Identifier(r5js.parse.Terminals.QUOTE);


/** @override */
r5js.ast.Quote.prototype.car = function() {
  return r5js.ast.Quote.CAR_;
};


/** @override */
r5js.ast.Quote.prototype.cdr = function() {
  return new r5js.ast.List(this.getFirstChild());
};


/** @override */
r5js.ast.Quote.prototype.fixParserSensitiveIds = goog.nullFunction;

/** @override */
r5js.ast.Quote.prototype.toProcCallLike = function() {
    return new r5js.QuoteShim_(this);
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
