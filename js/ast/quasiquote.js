goog.module('r5js.ast.Quasiquote');

const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const ContinuableHelper = goog.require('r5js.ContinuableHelper');
const Datum = goog.require('r5js.Datum');
const IEnvironment = goog.require('r5js.IEnvironment');
const Identifier = goog.require('r5js.ast.Identifier');
const Nonterminal = goog.require('r5js.parse.Nonterminal');
const Quote = goog.require('r5js.ast.Quote');
const Unquote = goog.require('r5js.ast.Unquote');
const UnquoteSplicing = goog.require('r5js.ast.UnquoteSplicing');
const {ProcCallLike} = goog.require('r5js.ProcCallLike');
const {Terminals} = goog.require('r5js.parse.Terminals');

const Nonterminals = Nonterminal.Nonterminals;

class Quasiquote extends CompoundDatum {
    /** @param {Datum} firstChild */
    constructor(firstChild) {
        super();
        if (firstChild) {
            this.setFirstChild(firstChild);
        }
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
                    parse(Nonterminals.EXPRESSION).
                    desugar(env, true)));
                /* Throw out the last result name and replace it with another
                 identifier (also illegal in Scheme) that will let us know if it's
                 unquotation or unquotation with splicing. */
                const name = (node instanceof Unquote
                        ? Terminals.COMMA
                        : Terminals.COMMA_AT) + '' + goog.getUid(new Object());
                const last = ProcCallLike.getLast(asContinuable);
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
     * @param {string=} opt_continuationName Optional name of the continuation.
     */
    constructor(payload, opt_continuationName) {
        super(opt_continuationName);
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
     * @return {ProcCallLike}
     * @private
     */
    tryQuasiquote_(quasiquote, parserProvider) {
        const continuable = quasiquote.processQuasiquote(
            /** @type {!IEnvironment} */ (this.getEnv()), parserProvider);
        const next = this.getNext();
        if (next) {
            ProcCallLike.appendProcCallLike(continuable, next);
        }
        return continuable;
    }
}

exports = Quasiquote;