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


/** @param {!r5js.IEnvironment} env */
r5js.ProcCallLike.prototype.setStartingEnv = function(env) {};


/**
 * @param {!r5js.ProcCallLike} procCallLike
 * @return {!r5js.ProcCallLike}
 */
r5js.ProcCallLike.getLast = function(procCallLike) {
  var continuation = procCallLike.getContinuation();
  var nextContinuable = continuation.getNextContinuable();
  return nextContinuable ?
      r5js.ProcCallLike.getLast(nextContinuable) :
      procCallLike;
};


/**
 * @param {!r5js.ProcCallLike} procCallLike
 * @param {!r5js.ProcCallLike} next The next continuable.
 */
r5js.ProcCallLike.appendContinuable = function(procCallLike, next) {
  r5js.ProcCallLike.getLast(procCallLike).
      getContinuation().
      setNextContinuable(next);
};

