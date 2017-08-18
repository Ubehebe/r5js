goog.module('r5js.ast.Lambda');

const Procedure = goog.require('r5js.Procedure');
const SimpleDatum = goog.require('r5js.ast.SimpleDatum');

/** @extends {SimpleDatum<!Procedure>} */
class Lambda extends SimpleDatum {
    /**
     * @param {string} name Name of the procedure.
     * @param {!Procedure} procedure TODO bl.
     */
    constructor(name, procedure) {
        super(procedure);
        /** @const @private */ this.name_ = name;
    }

    /** @return {string} */
    getName() {
        return this.name_;
    }
}

exports = Lambda;
