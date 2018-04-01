import {ProcCallResult} from "../ast/proc_call_like";
import {Parser} from "../parse/parser";
import {Environment} from "./environment";
import {ProcCallLike} from "../ast/proc_call_like";

export class Branch extends ProcCallLike {

  private readonly consequentLastContinuable: ProcCallLike;
  private readonly alternateLastContinuable: ProcCallLike;

  constructor(private readonly testResultName: string,
              private readonly consequent: ProcCallLike,
              private readonly alternate: ProcCallLike) {
    super();
    this.consequentLastContinuable = this.consequent.getLast();
    this.alternateLastContinuable = this.alternate.getLast();
  }

  /** @override */
  setStartingEnv(env: Environment) {
    if (!this.consequent.getEnv()) {
      this.consequent.setStartingEnv(env);
    }
    if (!this.alternate.getEnv()) {
      this.alternate.setStartingEnv(env);
    }
  }

  /** @override */
  getEnv(): Environment | null {
    return null;
  }

  /** @override */
  evalAndAdvance(resultStruct: ProcCallResult,
                 env: Environment,
                 parserProvider: (Datum) => Parser) {
    // Branches always use the old environment left by the previous action on the trampoline.
    const testResult = env.get(this.testResultName);
    if (testResult === false) {
      this.alternateLastContinuable.setNext(this.getNext()! /* TODO: incorrect? */);
      this.alternateLastContinuable.setResultName(this.getResultName());
      resultStruct.setNext(this.alternate);
      // We must clear the environment off the non-taken branch. See comment at
      // {@link r5js.Continuation.rememberEnv}. TODO bl: clearEnv is defined only on
      // {@link r5js.ProcCall}, yet all of the tests pass. This suggests either test coverage is
      // insufficient or that I don't understand the type of subtype.
      this.consequent.clearEnv();
    } else {
      this.consequentLastContinuable.setNext(this.getNext()!);
      this.consequentLastContinuable.setResultName(this.getResultName());
      resultStruct.setNext(this.consequent);
      // We must clear the environment off the non-taken branch. See comment at
      // {@link r5js.Continuation.rememberEnv}, and above.
      this.alternate.clearEnv();
    }
  }
}

