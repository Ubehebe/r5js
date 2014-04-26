goog.provide('r5js.ProcedureLike');



/**
 * @interface
 * @extends {r5js.runtime.ObjectValue}
 */
r5js.ProcedureLike = function() {};


/**
 * @param {!r5js.ProcCall} procCall
 * @param {!r5js.ProcCallLike} procCallLike
 * @param {!r5js.TrampolineHelper} trampolineHelper
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 */
r5js.ProcedureLike.prototype.evalAndAdvance = function(
    procCall, procCallLike, trampolineHelper, parserProvider) {};


/** @return {boolean} */
r5js.ProcedureLike.prototype.operandsMustBeInContinuationPassingStyle =
    function() {};


/** @const @private */
r5js.ProcedureLike.IMPLEMENTED_BY_PROP_ = '$r5js.ProcedureLike';


/**
 * @param {*} obj
 * @return {boolean}
 * TODO bl temporary shim, remove.
 */
r5js.ProcedureLike.isImplementedBy = function(obj) {
  return !!(obj && obj[r5js.ProcedureLike.IMPLEMENTED_BY_PROP_]);
};


/** @param {function(new: r5js.ProcedureLike, ...)} ctor */
r5js.ProcedureLike.addImplementation = function(ctor) {
  ctor.prototype[r5js.ProcedureLike.IMPLEMENTED_BY_PROP_] = true;
};
