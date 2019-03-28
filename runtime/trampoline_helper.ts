import {UNSPECIFIED_VALUE} from "../ast/datum";
import {ProcCallLike, ProcCallResult} from "../ast/proc_call_like";
import {Value} from "../base/value";
import {InputPort} from "../io/input_port";
import {OutputPort} from "../io/output_port";

export class TrampolineHelper implements ProcCallResult {

  private beforeThunk: ProcCallLike | null = null;
  private nextContinuable: ProcCallLike | null = null;
  private value: Value = UNSPECIFIED_VALUE;

  constructor(
      private readonly inputPort: InputPort,
      private readonly outputPort: OutputPort) {}

  /** Clears the object's state. TODO bl: not {@link beforeThunk}? */
  clear() {
    this.nextContinuable = null;
  }

  getBeforeThunk(): ProcCallLike | null {
    return this.beforeThunk;
  }

  setBeforeThunk(beforeThunk: ProcCallLike) {
    this.beforeThunk = beforeThunk;
  }

  /** @override */
  getNextProcCallLike() {
    return this.nextContinuable;
  }

  /** @override */
  setNext(procCallLike: ProcCallLike) {
    this.nextContinuable = procCallLike;
  }

  getValue(): Value {
    return this.value;
  }

  /** @override */
  setValue(value: Value) {
    this.value = value;
  }

  getInputPort(): InputPort {
    return this.inputPort;
  }

  getOutputPort(): OutputPort {
    return this.outputPort;
  }
}
