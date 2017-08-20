goog.module('r5js.TopLevelSyntaxAssignment');

const Datum = goog.require('r5js.Datum');
const Identifier = goog.require('r5js.ast.Identifier');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const TopLevelAssignment = goog.require('r5js.TopLevelAssignment');
const {ProcCallLike} = goog.require('r5js.ProcCallLike');

class TopLevelSyntaxAssignment extends TopLevelAssignment {
    /** @param {Datum} firstOperand */
    constructor(firstOperand) {
        super(firstOperand);
    }

    /** @override */
    checkForImproperSyntaxAssignment() {
    }

    /**
     * @param {string} dstName
     * @param {string} srcName
     * @return {!ProcCallLike}
     */
    static of(dstName, srcName) {
        const operands = new SiblingBuffer()
            .appendSibling(new Identifier(dstName))
            .appendSibling(new Identifier(srcName))
            .toSiblings();
        return new TopLevelSyntaxAssignment(operands);
    }
}

exports = TopLevelSyntaxAssignment;