import {ProcCall} from "./proc_call";
import {Identifier} from "../ast/identifier";
import {Continuation} from "./continuation";
import {Value} from "../value";

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
