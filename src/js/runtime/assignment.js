goog.module('r5js.Assignment');

const Datum = goog.require('r5js.Datum');
const Error = goog.require('r5js.Error');
const Identifier = goog.require('r5js.ast.Identifier');
const Macro = goog.require('r5js.Macro');
const ProcCallLike = goog.require('r5js.ProcCallLike');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const UNSPECIFIED_VALUE = goog.require('r5js.runtime.UNSPECIFIED_VALUE');
const Value = goog.require('r5js.runtime.Value');

class Assignment extends ProcCallLike {
    /** @param {Datum} firstOperand */
    constructor(firstOperand) {
        super();
        /** @const @private */ this.firstOperand_ = firstOperand;
    }

    /**
     * @override
     * @suppress {checkTypes} TODO bl
     */
    evalAndAdvance(resultStruct, envBuffer, parserProvider) {
        var src = this.getEnv().get(/** @type {string} */ (
            this.firstOperand_.getNextSibling().getPayload()));
        this.checkForImproperSyntaxAssignment(src);
        this.mutateEnv(/** @type {string} */ (this.firstOperand_.getPayload()), src);
        // R5RS 4.1.6: the value of an assignment is unspecified.
        resultStruct.setValue(UNSPECIFIED_VALUE);
        this.bindResult(UNSPECIFIED_VALUE);
        var nextContinuable = this.getNext();
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
     * @param {!r5js.runtime.Value} val
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
        const operands = new SiblingBuffer()
            .appendSibling(new Identifier(dstName))
            .appendSibling(new Identifier(srcName))
            .toSiblings();
        return new Assignment(operands);
    }

}


exports = Assignment;