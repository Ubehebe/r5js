goog.module('r5js.Procedure');

const IEnvironment = goog.require('r5js.IEnvironment');
const ProcCallLike = goog.require('r5js.ProcCallLike');
const TrampolineHelper = goog.require('r5js.TrampolineHelper');
const {ObjectValue, Value} = goog.require('r5js.Value');

/** @implements {ObjectValue} */
class Procedure {
    /**
     * @param {!Array<!Value>} args
     * @param {!ProcCallLike} procCall
     * @param {!TrampolineHelper} trampolineHelper
     * @param {!IEnvironment} env
     */
    evaluate(args, procCall, trampolineHelper, env) {
        return goog.abstractMethod();
    }
}

exports = Procedure;