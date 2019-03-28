import {Identifier} from "../ast/identifier";
import {Value} from "../base/value";
import {Continuation} from "./continuation";
import {ProcCall} from "./proc_call";

export class CallWithCurrentContinuation extends ProcCall {

  constructor(
      operatorName: Identifier,
      private readonly continuation: Continuation) {
    super(operatorName, null /* firstOperand */);
  }

  /** @override */
  evalArgs(): Value[] {
    return [this.continuation];
  }
}
