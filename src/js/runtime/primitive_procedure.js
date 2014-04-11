goog.provide('r5js.PrimitiveProcedure');



/**
 * @extends {r5js.runtime.ObjectValue}
 * @interface
 */
r5js.PrimitiveProcedure = function() {};


/**
 * @param {!goog.array.ArrayLike} userArgs
 * @param {!r5js.ProcCall} procCall
 * @param {!r5js.Continuation} continuation
 * @param {!r5js.TrampolineHelper} trampolineHelper
 */
r5js.PrimitiveProcedure.prototype.Call = function(
    userArgs, procCall, continuation, trampolineHelper) {};


/**
 * Procedures have no deep need to know their names, as they are only bindings
 * and can change: (set! car cdr). This method exists only to increase
 * the usefulness of error messages thrown from primitive procedures.
 * @param {string} name
 */
r5js.PrimitiveProcedure.prototype.setDebugName = function(name) {};


