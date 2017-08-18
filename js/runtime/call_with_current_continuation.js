goog.module('r5js.CallWithCurrentContinuation');

const Continuation = goog.require('r5js.Continuation');
const Identifier = goog.require('r5js.ast.Identifier');
const ProcCall = goog.require('r5js.ProcCall');

class CallWithCurrentContinuation extends ProcCall {
    /**
     * @param {!Identifier} operatorName
     * @param {!Continuation} continuation
     */
    constructor(operatorName, continuation) {
        super(operatorName, null /* firstOperand */);
        /** @const @private */ this.continuation_ = continuation;
    }

    /** @override */
    evalArgs() {
        return [this.continuation_];
    }
}

exports = CallWithCurrentContinuation;
