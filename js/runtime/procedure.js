goog.module('r5js.Procedure');

const {ProcCallLike, ProcCallResult} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');

/** @implements {ObjectValue} */
class Procedure {
    /**
     * @param {!Array<!Value>} args
     * @param {!ProcCallLike} procCall
     * @param {!ProcCallResult} resultStruct
     * @param {!IEnvironment} env
     */
    evaluate(args, procCall, resultStruct, env) {
        return goog.abstractMethod();
    }
}

exports = Procedure;