goog.module('r5js.TopLevelSyntaxAssignment');

const {Datum, ProcCallLike} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {Identifier} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');
const {SiblingBuffer} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/sibling_buffer');
const {TopLevelAssignment} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/top_level_assignment');

class TopLevelSyntaxAssignment extends TopLevelAssignment {
    /** @param {!Datum} firstOperand */
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
        const operands = /** @type {!Datum} */ (new SiblingBuffer()
            .appendSibling(new Identifier(dstName))
            .appendSibling(new Identifier(srcName))
            .toSiblings());
        return new TopLevelSyntaxAssignment(operands);
    }
}

exports = TopLevelSyntaxAssignment;