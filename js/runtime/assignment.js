goog.module('r5js.Assignment');

const {Macro} = require('/js/macro/shim_collect_es6_sources.es6/node_modules/__main__/js/macro/macro');
const {SiblingBuffer} = require('/js/macro/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/sibling_buffer');
const {Datum, ProcCallLike, UNSPECIFIED_VALUE} = require('/js/macro/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {Identifier} = require('/js/macro/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');
const {SimpleDatum} = require('/js/macro/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/simple_datum');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');

class Assignment extends ProcCallLike {
    /** @param {!Identifier} firstOperand */
    constructor(firstOperand) {
        super();
        /** @const @private */ this.firstOperand_ = firstOperand;
    }

    /** @override */
    evalAndAdvance(resultStruct, envBuffer, parserProvider) {
        // TODO: write out this type. src can be null.
        const src = /** @type {?} */ (this.getEnv().get(
            (/** @type {!SimpleDatum<string>} */ (this.firstOperand_.getNextSibling()))
            .getPayload()));
        this.checkForImproperSyntaxAssignment(src);
        this.mutateEnv(this.firstOperand_.getPayload(), src);
        // R5RS 4.1.6: the value of an assignment is unspecified.
        resultStruct.setValue(UNSPECIFIED_VALUE);
        this.bindResult(UNSPECIFIED_VALUE);
        const nextContinuable = this.getNext();
        if (nextContinuable) {
            resultStruct.setNext(nextContinuable);
        }
    }

    /**
     * In Scheme, macros can be bound to identifiers but they are not really
     * first-class citizens; you cannot say (define x let) because the text "let"
     * does not parse as an expression (at least if it has its normal binding).
     * In this implementation, however, macros are objects that go into and come
     * out of environments like any other kind of objects. All kinds of assignments
     * -- top-level, internal, syntax, non-syntax -- go through this method,
     * so we have to make sure we don't accidentally permit some illegal behavior.
     *
     * If we're trying to assign a {@link r5js.Macro} here, the programmer is
     * requesting this assignment and we ought to signal an error.
     *
     * @see {r5js.TopLevelSyntaxAssignment#checkForImproperSyntaxAssignment},
     * which bypasses this check.
     *
     * @param {!Value} val
     * @protected
     */
    checkForImproperSyntaxAssignment(val) {
        if (val instanceof Macro && !val.isLetOrLetrecSyntax()) {
            throw Error.internalInterpreterError('TODO bl');
        }
    }

    /**
     * @param {string} name
     * @param {!Value} val
     * @protected
     */
    mutateEnv(name, val) {
        this.getEnv().mutate(name, val, false /* isTopLevel */);
    }

    /**
     * @param {string} dstName
     * @param {string} srcName
     * @return {!ProcCallLike}
     */
    static create(dstName, srcName) {
        const operands = /** @type {!Datum} */ (new SiblingBuffer()
            .appendSibling(new Identifier(dstName))
            .appendSibling(new Identifier(srcName))
            .toSiblings());
        return new Assignment(operands);
    }

}

exports = Assignment;