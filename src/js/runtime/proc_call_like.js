goog.provide('r5js.ProcCallLike');



/**
 * @param {string=} opt_lastResultName
 * @struct
 * @constructor
 */
r5js.ProcCallLike = function(opt_lastResultName) {
  /** @private */ this.resultName_ = opt_lastResultName ||
      ('@' /* TODO bl document */ + goog.getUid(this));
  /** @private {r5js.ProcCallLike} */ this.next_ = null;
  /** @private {r5js.IEnvironment} */ this.env_ = null;
};


/**
 * @param {!r5js.TrampolineHelper} trampolineHelper
 * @param {!r5js.EnvBuffer} envBuffer
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 */
r5js.ProcCallLike.prototype.evalAndAdvance = goog.abstractMethod;


/** @return {string} */
r5js.ProcCallLike.prototype.getResultName = function() {
  return this.resultName_;
};


/**
 * @param {string} resultName
 * TODO bl remove.
 */
r5js.ProcCallLike.prototype.setResultName = function(resultName) {
  this.resultName_ = resultName;
};


/** @param {!r5js.IEnvironment} env */
r5js.ProcCallLike.prototype.setStartingEnv = function(env) {
  this.env_ = env;
};


/** @return {r5js.IEnvironment} */
r5js.ProcCallLike.prototype.getEnv = function() {
  return this.env_;
};


/** @return {r5js.ProcCallLike} */
r5js.ProcCallLike.prototype.getNext = function() {
  return this.next_;
};


/** @param {!r5js.ProcCallLike} next */
r5js.ProcCallLike.prototype.setNext = function(next) {
  this.next_ = next;
};


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

