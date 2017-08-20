goog.module('r5js.Procedure');

const IEnvironment = goog.require('r5js.IEnvironment');
const {ObjectValue, Value} = goog.require('r5js.Value');
const {ProcCallLike, ResultStruct} = goog.require('r5js.ProcCallLike');

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