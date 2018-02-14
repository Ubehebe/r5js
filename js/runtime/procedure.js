goog.module('r5js.Procedure');

const {ProcCallLike} = goog.require('r5js.ProcCallLike');
const {ProcCallResult: ResultStruct} = goog.require('r5js.ProcCallResult');

/** @implements {ObjectValue} */
class Procedure {
    /**
     * @param {!Array<!Value>} args
     * @param {!ProcCallLike} procCall
     * @param {!ResultStruct} resultStruct
     * @param {!IEnvironment} env
     */
    evaluate(args, procCall, resultStruct, env) {
        return goog.abstractMethod();
    }
}

exports = Procedure;