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

  /* Cut out the current proc call from the continuation chain to
     avoid an infinite loop. Example:

     (define cont #f)
     (display
     (call-with-current-continuation
     (lambda (c)
     (set! cont c)
     "inside continuation")))
     (cont "outside continuation")
     42

     This should display "inside continuation", then "outside continuation",
     then return 42. When the trampoline is at

     (cont "outside continuation")

     proc.nextContinuable will be something like

     (cont "outside continuation" _0 [_0 (id 42 [_1 ...])])

     We clearly have to cut out the first part of this chain to avoid an
     infinite loop. */
  for (var tmp = trampolineHelper.getNextProcCallLike(), prev;
      tmp;
      prev = tmp, tmp = tmp.getContinuation().nextContinuable_) {
    if (tmp === procCall) {
      if (prev) {
        prev.getContinuation().nextContinuable_ =
            tmp.getContinuation().nextContinuable_;
      }
      break;
    }
  }
};
