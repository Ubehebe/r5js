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


/** @const @private {string} */
r5js.PrimitiveProcedure.IMPLEMENTED_BY_PROP_ = '$r5js.PrimitiveProcedure';


/**
 * @param {*} obj
 * @return {boolean}
 * TODO temporary shim. Remove.
 */
r5js.PrimitiveProcedure.isImplementedBy = function(obj) {
  return !!obj && !!obj[r5js.PrimitiveProcedure.IMPLEMENTED_BY_PROP_];
};


/** @param {function(new: r5js.PrimitiveProcedure,...)} ctor */
r5js.PrimitiveProcedure.addImplementation = function(ctor) {
  ctor.prototype[r5js.PrimitiveProcedure.IMPLEMENTED_BY_PROP_] = true;
};



