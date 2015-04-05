goog.module('r5js.TopLevelAssignment');

const Assignment = goog.require('r5js.Assignment');
const Datum = goog.require('r5js.Datum');
const Identifier = goog.require('r5js.ast.Identifier');
const ProcCallLike = goog.require('r5js.ProcCallLike');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');

class TopLevelAssignment extends Assignment {
    constructor(firstOperand) {
        super(firstOperand);
    }

    /** @override */
    mutateEnv(name, val) {
        this.getEnv().mutate(name, val, true /* isTopLevel */);
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
    return new TopLevelAssignment(operands);
};
}

exports = TopLevelAssignment;