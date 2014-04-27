goog.provide('r5js.Procedure');



/**
 * @implements {r5js.runtime.ObjectValue}
 * @struct
 * @constructor
 */
r5js.Procedure = function() {};


/**
 * @param {!Array.<!r5js.runtime.Value>} args
 * @param {!r5js.ProcCallLike} procCall
 * @param {!r5js.TrampolineHelper} trampolineHelper
 */
r5js.Procedure.prototype.evaluate = goog.abstractMethod;
