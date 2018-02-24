goog.module('r5js.ast.Quasiquote');

const {CompoundDatum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/compound_datum');
const {ContinuableHelper} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/continuable_helper');
const {Datum, ProcCallLike, appendProcCallLike, getLastProcCallLike} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {Identifier} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');
const {Quote} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/quote');
const {Unquote} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/unquote');
const {UnquoteSplicing} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/unquote_splicing');
const {EXPRESSION} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');
const {COMMA, COMMA_AT} = require('/js/parse/terminals_collect_es6_sources.es6/node_modules/__main__/js/parse/terminals');

class Quasiquote extends CompoundDatum {
    /** @param {!Datum} firstChild */
    constructor(firstChild) {
        super();
        this.setFirstChild(firstChild);
    }

    /**
     * TODO bl: not sure this is the right thing to do for quasiquotes.
     * We can't just "unescape" the quasiquotations. Example:
     *
     * (equal? '(a `(b ,(+ 1 2))) '(a `(b ,(+ 1 2))))
     *
     * This will eventually call
     *
     * (eqv? `(b ,(+ 1 2)) `(b ,(+ 1 2)))
     *
     * From this procedure call, it looks as if we should unescape the quasiquotes,
     * but that's incorrect; we've lost the surrounding quotation level.
     * It may be possible to figure out what to do based on the qqLevels,
     * but it's been a while since I've looked at that subsystem.
     *
     * @override
     */
    eqv(other) {
        return this.isEqual(/** @type {!CompoundDatum} */ (other));
    }

    /**
     * Example: `(1 ,(+ 2 3)) should desugar as (+ 2 3 [_0 (id (1 _0) [_2 ...])])
     * @param {!IEnvironment} env TODO bl.
     * @param {function(!Datum):!r5js.Parser} parserProvider Function
     * that will return a new Parser for the given Datum when called.
     * @return {!ProcCallLike}
     */
    processQuasiquote(env, parserProvider) {
        const newCalls = new ContinuableHelper();
        const qqLevel = this.qqLevel;

        this.replaceChildren(
            function (node) {
                return (node instanceof Unquote || node instanceof UnquoteSplicing)
                    && node.getQQLevel() === qqLevel;
            },
            function (node) {
                node = /** @type {!CompoundDatum} */ (node); // TODO bl
                const asContinuable = (/** @type {!ProcCallLike} */ (parserProvider(
                    /** @type {!Datum} */(node.getFirstChild())).
                    parse(EXPRESSION).
                    desugar(env, true)));
                /* Throw out the last result name and replace it with another
                 identifier (also illegal in Scheme) that will let us know if it's
                 unquotation or unquotation with splicing. */
                const name = (node instanceof Unquote
                        ? COMMA
                        : COMMA_AT) + '' + goog.getUid(new Object());
                const last = getLastProcCallLike(asContinuable);
                last.setResultName(name);
                newCalls.appendProcCallLike(asContinuable);
                return new Identifier(name);
            });

        const newDatum = new Quote(this.getFirstChild());

        newCalls.appendProcCallLike(newDatum.toProcCallLike());
        return /** @type {!ProcCallLike} */ (newCalls.toContinuable());
    }

    /** @override */
    setQuasiquotationLevel(qqLevel) {
        this.qqLevel = qqLevel + 1;
        return super.setQuasiquotationLevel(this.qqLevel);
    }

    /** @override */
    toProcCallLike() {
        return new QuasiquoteShim(this);
    }
}

/** TODO bl the purpose of this class is unclear. */
class QuasiquoteShim extends ProcCallLike {
    /**
     * @param {!Quasiquote} payload
     * @param {string=} continuationName Optional name of the continuation.
     */
    constructor(payload, continuationName=undefined) {
        super(continuationName);
        /** @const @private */ this.firstOperand_ = payload;
    }

    /** @override */
    evalAndAdvance(resultStruct, env, parserProvider) {
        const next = this.tryQuasiquote_(this.firstOperand_, parserProvider);
        if (next) {
            resultStruct.setNext(next);
        }
    }

    /**
     * @param {!Quasiquote} quasiquote
     * @param {function(!Datum):!r5js.Parser} parserProvider
     * @return {!ProcCallLike}
     * @private
     */
    tryQuasiquote_(quasiquote, parserProvider) {
        const continuable = quasiquote.processQuasiquote(
            /** @type {!IEnvironment} */ (this.getEnv()), parserProvider);
        const next = this.getNext();
        if (next) {
            appendProcCallLike(continuable, next);
        }
        return continuable;
    }
}

exports = Quasiquote;