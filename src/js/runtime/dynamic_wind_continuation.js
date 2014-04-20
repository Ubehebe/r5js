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
  goog.base(this);
  /** @const @private */ this.thunk_ = thunk;
  if (nextProcCallLike) {
    this.setNextContinuable(nextProcCallLike);
  }
  this.setLastResultName(lastResultName);
};
goog.inherits(r5js.DynamicWindContinuation, r5js.Continuation);


/**
 * @override
 * @suppress {accessControls}
 */
r5js.DynamicWindContinuation.prototype.evalAndAdvance = function(
    procCall, continuation, trampolineHelper, parserProvider) {
  var arg = procCall.evalArgs(false)[0]; // there will only be 1 arg
  procCall.env.addBinding(this.lastResultName_, arg);
  trampolineHelper.setValue(arg);
  trampolineHelper.setNextProcCallLike(this.thunk_);
  if (this.nextContinuable_) {
    r5js.ProcCallLike.appendProcCallLike(this.thunk_, this.nextContinuable_);
  }
  r5js.Continuation.repairInfiniteLoop(procCall, trampolineHelper);
};
