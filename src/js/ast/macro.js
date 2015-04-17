goog.module('r5js.ast.Macro');

const SimpleDatum = goog.require('r5js.ast.SimpleDatum');

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

