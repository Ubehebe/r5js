goog.provide('r5js.CallWithCurrentContinuation');


goog.require('r5js.ProcCall');



/**
 * @param {!r5js.ast.Identifier} operatorName
 * @param {!r5js.Continuation} continuation
 * @extends {r5js.ProcCall}
 * @struct
 * @constructor
 */
r5js.CallWithCurrentContinuation = function(operatorName, continuation) {
  goog.base(this, operatorName, null /* firstOperand */);

  /** @const @private */ this.continuation_ = continuation;
};
goog.inherits(r5js.CallWithCurrentContinuation, r5js.ProcCall);


/** @override */
r5js.CallWithCurrentContinuation.prototype.evalArgs = function() {
  return [this.continuation_];
};
