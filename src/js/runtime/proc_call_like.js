goog.provide('r5js.ProcCallLike');



/** @interface */
r5js.ProcCallLike = function() {};


/**
 * @param {!r5js.Continuation} continuation
 * @param {!r5js.TrampolineHelper} trampolineHelper
 * @param {!r5js.EnvBuffer} envBuffer
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 */
r5js.ProcCallLike.prototype.evalAndAdvance = function(
    continuation, trampolineHelper, envBuffer, parserProvider) {};


/** @return {r5js.Continuation} */
r5js.ProcCallLike.prototype.getContinuation = function() {};


/** @param {!r5js.Continuation} continuation */
r5js.ProcCallLike.prototype.setContinuation = function(continuation) {};
