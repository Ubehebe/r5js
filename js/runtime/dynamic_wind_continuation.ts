import {Continuation} from "./continuation";
import {ProcCallResult} from "../ast/proc_call_like";
import {Value} from "../value";
import {ProcCallLike} from "../ast/proc_call_like";

/**
 * Just for call/ccs inside dynamic-winds.
 * TODO bl: document why we don't have to install the "after" thunk.
 * (I'm pretty sure the reason is it's already in the continuable chain
 * somewhere.)
 */
export class DynamicWindContinuation extends Continuation {
  constructor(
      private readonly thunk: ProcCallLike,
      nextProcCallLike: ProcCallLike,
      lastResultName: string) {
    super(lastResultName, nextProcCallLike);
  }

  /** @override */
  evaluate(arg: Value, procCallLike: ProcCallLike, resultStruct: ProcCallResult) {
    procCallLike.getEnv()!.addBinding(this.lastResultName, arg);
    resultStruct.setValue(arg);
    resultStruct.setNext(this.thunk);
    if (this.nextContinuable) {
      this.thunk.append(this.nextContinuable);
    }
    Continuation.repairInfiniteLoop(procCallLike, resultStruct);
  }
}
