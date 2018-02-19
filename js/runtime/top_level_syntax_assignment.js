goog.module('r5js.TopLevelSyntaxAssignment');

const {Datum, ProcCallLike} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const Identifier = goog.require('r5js.ast.Identifier');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const TopLevelAssignment = goog.require('r5js.TopLevelAssignment');

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