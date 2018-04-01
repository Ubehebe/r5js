import {SimpleDatum} from "./simple_datum";
import {CPS_PREFIX, isParserSensitiveId} from "../parse/rename_util";
import {RenameHelper} from "./rename_helper";
import {ProcCallResult} from "./proc_call_like";
import {COMMA} from "../parse/terminals";
import {Environment} from "../runtime/environment";
import {Value} from "../value";
import {ProcCallLike} from "./proc_call_like";

export class Identifier extends SimpleDatum<string> {
  constructor(name: string) {
    super(name);
  }

  /**
   * @return True iff this Datum is in a quasiquotation and should be
   * unquoted (i.e. starts with a `,`).
   */
  shouldUnquote(): boolean {
    return this.payload.charAt(0) === COMMA;
  }

  /**
   * This is a subcase of shouldUnquote, because unquotes and unquote-splicings
   * have pretty much the same logic.
   * @return {boolean} TODO bl.
   */
  shouldUnquoteSplice(): boolean {
    return this.payload.charAt(1) === CPS_PREFIX;
  }

  /** @override */
  fixParserSensitiveIds(helper: RenameHelper) {
    if (isParserSensitiveId(this.payload)) {
      const renamedAs = helper.getRenameBinding(this.payload);
      if (renamedAs) {
        this.setPayload(renamedAs);
      }
    }
  }

  /** @override */
  toProcCallLike(): ProcCallLike {
    return new IdShim(this);
  }
}

/**
 * If a nonterminal in the grammar has no associated desugar function,
 * desugaring it will be a no-op. That is often the right behavior,
 * but sometimes we would like to wrap the datum in a Continuable
 * object for convenience on the trampoline. For example, the program
 * "1 (+ 2 3)" should be desugared as (id 1 [_0 (+ 2 3 [_1 ...])]).
 *
 * We represent these id shims as ProcCalls whose operatorNames are null
 * and whose firstOperand is the payload.
 */
class IdShim extends ProcCallLike {

  private readonly firstOperand: Identifier;

  constructor(payload: Identifier, continuationName = undefined) {
    super(continuationName);
    this.firstOperand = payload;
  }

  /** @override */
  evalAndAdvance(resultStruct: ProcCallResult, env: Environment, parserProvider: (x: any) => any) {
    const ans = this.tryIdentifier_(this.firstOperand);

    if (ans !== null) {
      this.bindResult(ans);
      resultStruct.setValue(ans);
    }

    const nextContinuable = this.getNext();

    if (nextContinuable) {
      resultStruct.setNext(nextContinuable);
    }
  }

  tryIdentifier_(id: Identifier): Value | null {
    let env = this.getEnv();
    return env && env.get(id.getPayload());
  }
}