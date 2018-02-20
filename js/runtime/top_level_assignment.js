goog.module('r5js.TopLevelAssignment');

const Assignment = goog.require('r5js.Assignment');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const {Identifier} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');
const {ProcCallLike} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');

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