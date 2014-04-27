goog.provide('r5js.AbstractProcedure');



/**
 * @implements {r5js.runtime.ObjectValue}
 * @struct
 * @constructor
 */
r5js.AbstractProcedure = function() {};


/**
 * @param {!Array.<!r5js.runtime.Value>} args
 * @param {!r5js.ProcCallLike} procCall
 * @param {!r5js.TrampolineHelper} trampolineHelper
 */
r5js.AbstractProcedure.prototype.evaluate = goog.abstractMethod;
