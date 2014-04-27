goog.provide('r5js.ProcCallLike');



/** @interface */
r5js.ProcCallLike = function() {};


/**
 * @param {!r5js.TrampolineHelper} trampolineHelper
 * @param {!r5js.EnvBuffer} envBuffer
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 */
r5js.ProcCallLike.prototype.evalAndAdvance = function(
    trampolineHelper, envBuffer, parserProvider) {};


/** @return {string} */
r5js.ProcCallLike.prototype.getResultName = function() {};


/**
 * @param {string} resultName
 * TODO bl remove.
 */
r5js.ProcCallLike.prototype.setResultName = function(resultName) {};


/** @param {!r5js.IEnvironment} env */
r5js.ProcCallLike.prototype.setStartingEnv = function(env) {};


/** @return {r5js.IEnvironment} */
r5js.ProcCallLike.prototype.getEnv = function() {};


/** @return {r5js.ProcCallLike} */
r5js.ProcCallLike.prototype.getNext = function() {};


/** @param {!r5js.ProcCallLike} next */
r5js.ProcCallLike.prototype.setNext = function(next) {};


/**
 * @param {!r5js.ProcCallLike} procCallLike
 * @return {!r5js.ProcCallLike}
 */
r5js.ProcCallLike.getLast = function(procCallLike) {
  var maybeNext = procCallLike.getNext();
  return maybeNext ? r5js.ProcCallLike.getLast(maybeNext) : procCallLike;
};


/**
 * @param {!r5js.ProcCallLike} procCallLike
 * @param {!r5js.ProcCallLike} next The next continuable.
 */
r5js.ProcCallLike.appendProcCallLike = function(procCallLike, next) {
  r5js.ProcCallLike.getLast(procCallLike).setNext(next);
};
