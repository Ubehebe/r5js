goog.module('r5js.ProcCall');

const ContinuableHelper = goog.require('r5js.ContinuableHelper');
const Continuation = goog.require('r5js.Continuation');
const Datum = goog.require('r5js.Datum');
const Identifier = goog.require('r5js.ast.Identifier');
const Lambda = goog.require('r5js.Lambda');
const Literal = goog.require('r5js.ast.Literal');
const Macro = goog.require('r5js.Macro');
const Parser = goog.require('r5js.Parser');
const Procedure = goog.require('r5js.Procedure');
const Quasiquote = goog.require('r5js.ast.Quasiquote');
const Quote = goog.require('r5js.ast.Quote');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const UNSPECIFIED_VALUE = goog.require('r5js.UNSPECIFIED_VALUE');
const Vector = goog.require('r5js.ast.Vector');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');
const {List} = goog.require('r5js.ast.List');
const {ProcCallLike, ResultStruct} = goog.require('r5js.ProcCallLike');
const {notAProcedure} = goog.require('r5js.runtime.errors');

class ProcCall extends ProcCallLike {
    /**
     * @param {!Identifier} operatorName
     * @param {?Datum} firstOperand
     * @param {string=} lastResultName Optional name to use for the last result.
     *     If not given, a unique name will be created.
     */
    constructor(operatorName, firstOperand, lastResultName=undefined) {
        super(lastResultName);
        /** @const @private */ this.operatorName_ = operatorName;
        /** @const @private */ this.firstOperand_ = firstOperand;
    }

    /** @return {?} TODO bl. */
    getFirstOperand() {
        return this.firstOperand_;
    }

    /**
     * @return {!Datum}
     * @private
     */
    reconstructDatum_() {
        const op = new Identifier(this.operatorName_.getPayload());
        if (this.firstOperand_) {
            op.setNextSibling(this.firstOperand_);
        }
        return new SiblingBuffer().appendSibling(op).toList(List);
    }

    /**
     * @return {boolean} True iff the operands are in continuation-passing style.
     * @private
     */
    operandsInContinuationPassingStyle_() {
        for (let cur = this.firstOperand_; cur; cur = cur.getNextSibling()) {
            if (cur instanceof Datum) {
                if (cur instanceof List && !cur.getFirstChild()) {
                    throw Error.illegalEmptyApplication(
                        /** @type {string} */ (this.operatorName_.getPayload()));
                } else if (!(cur instanceof Literal
                    || cur instanceof Quote
                    || cur instanceof Vector)) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * If the operator resolves as a primitive or non-primitive procedure,
     * check that the operands are simple. If they're not, rearrange the flow
     * of control to compute them first.
     *
     * Example: (+ (* 2 3) (/ 4 5)) will need to be turned into something like
     *
     * (* 2 3 [_0 (/ 4 5 [_1 (+ _0 _1 [...])])])
     *
     * (We do _not_ do this if the operator resolves as a macro. Macros
     * get their arguments as unevaluated datums.)
     *
     * @param {!ResultStruct} resultStruct
     * @param {function(!Datum):!Parser} parserProvider Function
     * that will return a new Parser for the given Datum when called.
     * @private
     */
    cpsify_(resultStruct, parserProvider) {
        const newCallChain = new ContinuableHelper();
        const finalArgs = new SiblingBuffer();
        let maybeContinuable;

        for (let arg = this.firstOperand_; arg; arg = arg.getNextSibling()) {
            arg.resetDesugars();
            if (arg instanceof Quote) {
                finalArgs.appendSibling(arg.clone(null /* parent */));
            } else if (arg instanceof Quasiquote) {
                maybeContinuable = arg.processQuasiquote(
                    /** @type {!IEnvironment} */ (this.getEnv()), parserProvider);
                finalArgs.appendSibling(
                    new Identifier(ProcCallLike.getLast(
                        maybeContinuable).getResultName()));
                newCallChain.appendProcCallLike(maybeContinuable);
            } else if (arg.isImproperList()) {
                throw Error.internalInterpreterError('TODO bl');
            } else if ((maybeContinuable = arg.desugar(
                    /** @type {!IEnvironment} */ (this.getEnv()))) instanceof ProcCallLike) {
                /* todo bl is it an invariant violation to be a list
                 and not to desugar to a Continuable? */
                const procCallLike = /** @type {!ProcCallLike} */ (maybeContinuable);
                finalArgs.appendSibling(
                    new Identifier(ProcCallLike.getLast(procCallLike).getResultName()));
                newCallChain.appendProcCallLike(procCallLike);
            } else {
                const clonedArg = arg.clone(null /* parent */);
                finalArgs.appendSibling(clonedArg);
            }
        }

        newCallChain.appendProcCallLike(
            new ProcCall(this.operatorName_, finalArgs.toSiblings()));

        const ans = /** @type {!ProcCallLike} */ (newCallChain.toContinuable());
        const lastContinuable = ProcCallLike.getLast(ans);
        const next = this.getNext();
        if (next) {
            lastContinuable.setNext(next);
        }
        lastContinuable.setResultName(this.getResultName());
        resultStruct.setNext(ans);
    }

    /** @override */
    evalAndAdvance(resultStruct, env, parserProvider) {
        const proc = this.getEnv().getProcedure(/** @type {string} */ (
            this.operatorName_.getPayload()));

        if (proc instanceof Procedure) {
            if (!this.operandsInContinuationPassingStyle_()) {
                this.cpsify_(resultStruct, parserProvider);
            } else {
                const args = this.evalArgs();
                proc.evaluate(args, this, resultStruct, env);
            }
        } else if (proc instanceof Macro) {
            const rawDatum = this.reconstructDatum_();
            proc.evaluate(rawDatum, this, resultStruct, parserProvider);
        } else if (proc instanceof Continuation) {
            const fakeArg = this.evalArgs()[0]; // TODO bl
            proc.evaluate(fakeArg, this, resultStruct);
        } else if (proc instanceof Datum) {
            throw notAProcedure(this.operatorName_.getPayload());
        } else {
            throw Error.internalInterpreterError("ProcCall: don't know what to do with " + proc);
        }
    }

    /**
     * @return {!Array<!Value>}
     * TODO bl: this method is confused.
     */
    evalArgs() {
        let maybeArray;
        if (maybeArray = this.evalArgsCallWithValues_()) {
            return maybeArray;
        }

        const args = [];

        for (let cur = this.firstOperand_; cur; cur = cur.getNextSibling()) {
            if (cur instanceof Identifier) {
                const name = cur.getPayload();
                const toPush = this.getEnv().get(name);
                /* Macros are not first-class citizens in Scheme; they cannot
                 be passed as arguments. Internally, however, we do just that
                 for convenience. The isLetOrLetrecSyntax flag discriminates
                 between the programmer and the implementation. */
                if (toPush instanceof Macro
                    && !toPush.isLetOrLetrecSyntax()) {
                    throw Error.macro(name, 'bad syntax');
                }
                // TODO bl this doesn't seem like the right behavior. Investigate.
                args.push(toPush === null ? UNSPECIFIED_VALUE : toPush);
            } else if (cur instanceof Quote) {
                args.push(cur.getFirstChild());
            } else if (cur instanceof Lambda) {
                args.push(cur);
            } else if (cur instanceof Datum) {
                args.push(cur.clone(null /* parent */));
            } else {
                throw Error.internalInterpreterError('unexpected datum ' + cur);
            }
        }

        return args;
    }

    /**
     * Special logic for values and call-with-values. Example:
     *
     * (call-with-values (lambda () (values 1 2 3)) +)
     *
     * The "producer" procedure, (lambda () (values 1 2 3)), will desugar to
     * something like
     *
     * (values 1 2 3 [_0 ...])
     *
     * In this implementation, this will bind the JavaScript array [1, 2, 3] to _0.
     * Later on the trampoline, we reach (+ _0). We have to know that _0 refers
     * to an array of values, not a single value.
     *
     * @return {?Array<!Value>}
     * @private
     */
    evalArgsCallWithValues_() {
        if (this.firstOperand_ instanceof Identifier
            && !this.firstOperand_.getNextSibling()) {
            const maybeArray = this.getEnv().get(
                /** @type {string} */ (this.firstOperand_.getPayload()));
            if (maybeArray instanceof Array) {
                return maybeArray;
            }
        }
        return null;
    }
}

exports = ProcCall;
