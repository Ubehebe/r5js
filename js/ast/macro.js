goog.module('r5js.ast.Macro');

const {SimpleDatum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/simple_datum');

class Macro extends SimpleDatum {
    /** @param {!r5js.Macro} macro */
    constructor(macro) {
        super(macro);
    }

    /** @return {!r5js.Macro} */
    getMacro() {
        return this.payload.setIsLetOrLetrecSyntax();
    }
}

exports = Macro;

