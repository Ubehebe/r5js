goog.module('r5js.TopLevelAssignment');

const Assignment = goog.require('r5js.Assignment');
const Identifier = goog.require('r5js.ast.Identifier');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const {ProcCallLike} = require('/js/runtime/proc_call_like_collect_es6_sources.es6/node_modules/__main__/js/runtime/proc_call_like');

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