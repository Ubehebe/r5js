goog.module('r5js.ast.Identifier');

const Datum = goog.require('r5js.Datum');
const ProcCallLike = goog.require('r5js.ProcCallLike');
const RenameUtil = goog.require('r5js.RenameUtil');
const SimpleDatum = goog.require('r5js.ast.SimpleDatum');
const Terminals = goog.require('r5js.parse.Terminals');
const Value = goog.require('r5js.runtime.Value');

/** @extends {SimpleDatum<string>} */
class Identifier extends SimpleDatum {
    /** @param {string} name */
    constructor(name) {
        super(name);
    }

    /**
     * @return {boolean} True iff this Datum is in a quasiquotation and should be
     * unquoted (i.e. starts with a ,).
     */
    shouldUnquote() {
        return this.payload.charAt(0) === Terminals.COMMA;
    }

    /**
     * This is a subcase of shouldUnquote, because unquotes and unquote-splicings
     * have pretty much the same logic.
     * @return {boolean} TODO bl.
     */
    shouldUnquoteSplice() {
        return this.payload.charAt(1) === RenameUtil.CPS_PREFIX;
    }

    /** @override */
    fixParserSensitiveIds(helper) {
        if (RenameUtil.isParserSensitiveId(this.payload)) {
            const renamedAs = helper.getRenameBinding(this.payload);
            if (renamedAs) {
                this.setPayload(renamedAs);
            }
        }
    }

    /** @override */
    toProcCallLike() {
        return new IdShim(this);
    }
}

/**
 * If a nonterminal in the grammar has no associated desugar function,
 * desugaring it will be a no-op. That is often the right behavior,
 * but sometimes we would like to wrap the datum in a Continuable
 * object for convenience on the trampoline. For example, the program
 * "1 (+ 2 3)" should be desugared as (id 1 [_0 (+ 2 3 [_1 ...])]).
 *
 * We represent these id shims as ProcCalls whose operatorNames are null
 * and whose firstOperand is the payload.
 */
class IdShim extends ProcCallLike {
    /**
     * @param {!Identifier} payload
     * @param {string=} opt_continuationName Optional name of the continuation.
     */
    constructor(payload, opt_continuationName) {
        super(opt_continuationName);
        /** @const @private */ this.firstOperand_ = payload;
    }

    /** @override */
    evalAndAdvance(resultStruct, env, parserProvider) {
        const ans = this.tryIdentifier_(this.firstOperand_);

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
     * @param {!Identifier} id
     * @return {?Value}
     */
    tryIdentifier_(id) {
        return this.getEnv().get(/** @type {string} */ (id.getPayload()));
    }
}

exports = Identifier;