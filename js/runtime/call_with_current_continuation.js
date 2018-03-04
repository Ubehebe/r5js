goog.module('r5js.CallWithCurrentContinuation');

const {Continuation} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/continuation');
const {ProcCall} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/proc_call');
const {Identifier} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');

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
