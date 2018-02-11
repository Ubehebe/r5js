goog.module('r5js.ast.Quote');

const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const Identifier = goog.require('r5js.ast.Identifier');
const Ref = goog.require('r5js.Ref');
const UNSPECIFIED_VALUE = goog.require('r5js.UNSPECIFIED_VALUE');
const datumutil = goog.require('r5js.datumutil');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');
const {IPair, addImplementation: addPairImpl} = require('/js/ast/ipair_collect_es6_sources.es6/node_modules/__main__/js/ast/ipair');
const {List} = goog.require('r5js.ast.List');
const {ProcCallLike} = goog.require('r5js.ProcCallLike');
const {Terminals} = goog.require('r5js.parse.Terminals');

class Quote extends CompoundDatum /* implicitly implements IPair (structural interface) */ {
    /** @param {?Datum} firstChild */
    constructor(firstChild) {
        super();
        if (firstChild) {
            this.setFirstChild(firstChild.setImmutable());
        }
    }

    /** @return {!Value} */
    car() {
        return CAR_;
    }

    /** @return {!Value} */
    cdr() {
        return new List(this.getFirstChild());
    }

    /** @override */
    fixParserSensitiveIds() {
    }

    /** @override */
    toProcCallLike() {
        return new QuoteShim(this);
    }
}

addPairImpl(Quote);

/** @const @private {!Value} */
const CAR_ = new Identifier(Terminals.QUOTE);


/** TODO bl the purpose of this class is unclear. */
class QuoteShim extends ProcCallLike {
    /**
     * @param {!Quote} payload
     * @param {string=} continuationName Optional name of the continuation.
     */
    constructor(payload, continuationName=undefined) {
        super(continuationName);
        /** @const @private */ this.firstOperand_ = payload;
    }

    /** @override */
    evalAndAdvance(resultStruct, env, parserProvider) {
        const ans = this.tryQuote_(this.firstOperand_);
        if (ans !== null) {
            this.bindResult(ans);
            resultStruct.setValue(ans);
        }

        const nextContinuable = this.getNext();

        if (nextContinuable) {
            resultStruct.setNext(nextContinuable);
        }
    }

    /**
     * @param {!Quote} quote
     * @return {?Value}
     * @private
     */
    tryQuote_(quote) {
        const env = this.getEnv();
        // Do the appropriate substitutions.
        const ans = quote.replaceChildren(
            function (node) {
                return node instanceof Identifier && node.shouldUnquote();
            },
            function (node) {
                const result = env.get(/** @type {string} */ ((/** @type {!Identifier} */ (node))
                    .getPayload()));
                /** @type {?Datum} */ let ans;
                ans = result === null
                    ? /** @type {!Datum} */ (UNSPECIFIED_VALUE)
                    : datumutil.wrapValue(result);
                // TODO bl document why we're doing this
                if (ans instanceof Ref) {
                    ans = ans.deref();
                }
                if (node instanceof Identifier && node.shouldUnquoteSplice()) {
                    if (ans instanceof List) {
                        if (ans.getFirstChild()) { // `(1 ,@(list 2 3) 4) => (1 2 3 4)
                            ans = ans.getFirstChild();
                        } else { // `(1 ,@(list) 2) => (1 2)
                            ans = null;
                        }
                    } else throw Error.quasiquote(ans + ' is not a list');
                }
                return ans;
            });
        // Now strip away the quote mark.
        // the newIdOrLiteral part is for (quote quote)
        return (ans instanceof CompoundDatum && ans.getFirstChild())
            ? ans.getFirstChild()
            : new Identifier(Terminals.QUOTE);
    }
}

exports = Quote;
