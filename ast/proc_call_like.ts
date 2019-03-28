import {Value} from "../base/value";
import {Environment} from "../runtime/environment";
import {Datum} from "./datum";

export interface ProcCallResult {
  setNext(procCallLike: ProcCallLike): void;
  setValue(value: Value): void;
  getNextProcCallLike(): ProcCallLike | null;
}

export abstract class ProcCallLike {

  protected resultName: string;
  protected next: ProcCallLike | null;
  protected env: Environment | null;

  constructor(lastResultName = `@${counter++}`) {
    this.resultName = lastResultName;
    this.next = null;
    this.env = null;
  }

  /**@param parserProvider Function that will return a new Parser for the given Datum when called. */
  abstract evalAndAdvance(resultStruct: ProcCallResult /* TODO */,
                          env: Environment,
                          parserProvider: (datum: Datum) => any /* TODO should be Parser*/): void;

  getResultName(): string {
    return this.resultName;
  }

  /** TODO bl remove. */
  setResultName(resultName: string) {
    this.resultName = resultName;
  }

  setStartingEnv(env: Environment) {
    this.env = env;
  }

  getEnv(): Environment | null {
    return this.env;
  }

  /** Clears the current environment. TODO bl not well understood. */
  clearEnv() {
    this.env = null;
  }

  getNext(): ProcCallLike | null {
    return this.next;
  }

  setNext(next: ProcCallLike) {
    this.next = next;
  }

  bindResult(val: Value) {
    // If the next procedure call already has an environment, bind the result there. Otherwise,
    // bind it in the current environment; it will be carried forward by the EnvBuffer.
    const envToUse: Environment|null = (this.next && this.next.getEnv()) || this.env;
    envToUse && envToUse.addBinding(this.resultName, val);
  }

  getLast(): ProcCallLike {
    const maybeNext = this.getNext();
    return maybeNext ? maybeNext.getLast() : this;
  }

  append(last: ProcCallLike) {
    this.getLast().setNext(last);
  }
}

let counter = 0;
