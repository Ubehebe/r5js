goog.module('r5js.Procedure');

const {ProcCallLike, ProcCallResult} = require('/js/runtime/proc_call_like_collect_es6_sources.es6/node_modules/__main__/js/runtime/proc_call_like');

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