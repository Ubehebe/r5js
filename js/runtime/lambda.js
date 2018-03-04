goog.module('r5js.Lambda');

const {Procedure} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/procedure');
const {SimpleDatum} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/simple_datum');

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
