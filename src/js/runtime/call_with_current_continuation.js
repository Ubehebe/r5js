goog.provide('r5js.CallWithCurrentContinuation');

goog.require('r5js.ProcCall');

r5js.CallWithCurrentContinuation = class extends r5js.ProcCall {
    /**
     * @param {!r5js.ast.Identifier} operatorName
     * @param {!r5js.Continuation} continuation
     */
    constructor(operatorName, continuation) {
        super(operatorName, null /* firstOperand */);
        /** @const @private */ this.continuation_ = continuation;
    }

    /** @override */
    evalArgs() {
        return [this.continuation_];
    }
};
