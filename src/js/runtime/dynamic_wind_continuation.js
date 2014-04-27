goog.provide('r5js.DynamicWindContinuation');


goog.require('r5js.Continuation');
goog.require('r5js.ProcCallLike');



/**
 * Just for call/ccs inside dynamic-winds.
 * TODO bl: document why we don't have to install the "after" thunk.
 * (I'm pretty sure the reason is it's already in the continuable chain
 * somewhere.)
 * @param {!r5js.ProcCallLike} thunk
 * @param {r5js.ProcCallLike} nextProcCallLike
 * @param {string} lastResultName
 * @extends {r5js.Continuation}
 * @struct
 * @constructor
 */
r5js.DynamicWindContinuation = function(
    thunk, nextProcCallLike, lastResultName) {
  goog.base(this, lastResultName, nextProcCallLike);
  /** @const @private */ this.thunk_ = thunk;
};
goog.inherits(r5js.DynamicWindContinuation, r5js.Continuation);


/**
 * @override
 * @suppress {accessControls} TODO bl fix
 */
r5js.DynamicWindContinuation.prototype.evalAndAdvance = function(
    procCall, procCallLike, trampolineHelper, parserProvider) {
  var arg = procCall.evalArgs()[0]; // there will only be 1 arg
  procCall.getEnv().addBinding(this.lastResultName_, arg);
  trampolineHelper.setValue(arg);
  trampolineHelper.setNext(this.thunk_);
  if (this.nextContinuable_) {
    r5js.ProcCallLike.appendProcCallLike(this.thunk_, this.nextContinuable_);
  }
  r5js.Continuation.repairInfiniteLoop(procCallLike, trampolineHelper);
};
