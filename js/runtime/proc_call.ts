import {Datum, getLastProcCallLike, ProcCallLike, ProcCallResult, UNSPECIFIED_VALUE} from "../ast/datum";
import {Identifier} from "../ast/identifier";
import {SiblingBuffer} from "../ast/sibling_buffer";
import {List} from "../ast/list";
import {SimpleDatum} from "../ast/simple_datum";
import {Quote} from "../ast/quote";
import {Vector} from "../ast/vector";
import {Error} from "../error";
import {ContinuableHelper} from "../ast/continuable_helper";
import {Quasiquote} from "../ast/quasiquote";
import {Procedure} from "./procedure";
import {Macro} from "../macro/macro";
import {Continuation} from "./continuation";
import {notAProcedure} from "./errors";
import {Parser} from "../parse/parser";
import {Lambda} from "./lambda";
import {Environment} from "./environment";
import {Value} from "../value";

export class ProcCall extends ProcCallLike {


  /**
   * @param lastResultName Optional name to use for the last result.
   *     If not given, a unique name will be created.
   */
  constructor(
      private readonly operatorName: Identifier,
      private readonly firstOperand: Datum | null,
      lastResultName: string | undefined = undefined) {
    super(lastResultName);
  }

  getFirstOperand(): Datum | null {
    return this.firstOperand;
  }

  private reconstructDatum(): Datum {
    const op = new Identifier(this.operatorName.getPayload());
    if (this.firstOperand) {
      op.setNextSibling(this.firstOperand);
    }
    return new SiblingBuffer().appendSibling(op).toList(List);
  }

  /** @return True iff the operands are in continuation-passing style. */
  private operandsInContinuationPassingStyle(): boolean {
    for (let cur = this.firstOperand; cur; cur = cur.getNextSibling()) {
      if (cur instanceof Datum) {
        if (cur instanceof List && !cur.getFirstChild()) {
          throw Error.illegalEmptyApplication(this.operatorName.getPayload());
        } else if (!(cur instanceof SimpleDatum
                || cur instanceof Quote
                || cur instanceof Vector)) {
          return false;
        }
      }
    }
    return true;
  }

  /**
   * If the operator resolves as a primitive or non-primitive procedure,
   * check that the operands are simple. If they're not, rearrange the flow
   * of control to compute them first.
   *
   * Example: (+ (* 2 3) (/ 4 5)) will need to be turned into something like
   *
   * (* 2 3 [_0 (/ 4 5 [_1 (+ _0 _1 [...])])])
   *
   * (We do _not_ do this if the operator resolves as a macro. Macros
   * get their arguments as unevaluated datums.)
   */
  private cpsify(resultStruct: ProcCallResult, parserProvider: (Datum) => Parser) {
    const newCallChain = new ContinuableHelper();
    const finalArgs = new SiblingBuffer();
    let maybeContinuable;

    for (let arg = this.firstOperand; arg; arg = arg.getNextSibling()) {
      arg.resetDesugars();
      if (arg instanceof Quote) {
        finalArgs.appendSibling(arg.clone(null /* parent */));
      } else if (arg instanceof Quasiquote) {
        maybeContinuable = arg.processQuasiquote(this.getEnv()!, parserProvider);
        finalArgs.appendSibling(
            new Identifier(getLastProcCallLike(
                maybeContinuable).getResultName()));
        newCallChain.appendProcCallLike(maybeContinuable);
      } else if (arg.isImproperList()) {
        throw Error.internalInterpreterError('TODO bl');
      } else if ((maybeContinuable = arg.desugar(this.getEnv()!)) instanceof ProcCallLike) {
        // TODO: is it an invariant violation to be a list and not to desugar to a Continuable?
        const procCallLike = /** @type {!ProcCallLike} */ (maybeContinuable);
        finalArgs.appendSibling(
            new Identifier(getLastProcCallLike(procCallLike).getResultName()));
        newCallChain.appendProcCallLike(procCallLike);
      } else {
        const clonedArg = arg.clone(null /* parent */);
        finalArgs.appendSibling(clonedArg);
      }
    }

    newCallChain.appendProcCallLike(
        new ProcCall(this.operatorName, finalArgs.toSiblings()));

    const ans = newCallChain.toContinuable()!;
    const lastContinuable = getLastProcCallLike(ans);
    const next = this.getNext();
    if (next) {
      lastContinuable.setNext(next);
    }
    lastContinuable.setResultName(this.getResultName());
    resultStruct.setNext(ans);
  }

  /** @override */
  evalAndAdvance(
      resultStruct: ProcCallResult,
      env: Environment,
      parserProvider: (Datum) => Parser) {
    const proc = this.getEnv()!.getProcedure(this.operatorName.getPayload());
    if (proc instanceof Procedure) {
      if (!this.operandsInContinuationPassingStyle()) {
        this.cpsify(resultStruct, parserProvider);
      } else {
        const args = this.evalArgs();
        proc.evaluate(args, this, resultStruct, env);
      }
    } else if (proc instanceof Macro) {
      const rawDatum = this.reconstructDatum();
      (proc as Macro).evaluate(rawDatum, this, resultStruct, parserProvider);
    } else if (proc instanceof Continuation) {
      const fakeArg = this.evalArgs()[0]; // TODO bl
      proc.evaluate(fakeArg, this, resultStruct);
    } else if (proc instanceof Datum) {
      throw notAProcedure(this.operatorName.getPayload());
    } else {
      throw Error.internalInterpreterError("ProcCall: don't know what to do with " + proc);
    }
  }

  /** TODO bl: this method is confused. */
  evalArgs(): Value[] {
    let maybeArray;
    if (maybeArray = this.evalArgsCallWithValues()) {
      return maybeArray;
    }

    const args: Value[] = [];

    for (let cur = this.firstOperand; cur; cur = cur.getNextSibling()) {
      if (cur instanceof Identifier) {
        const name = cur.getPayload();
        const toPush = this.getEnv()!.get(name);
        /* Macros are not first-class citizens in Scheme; they cannot
         be passed as arguments. Internally, however, we do just that
         for convenience. The isLetOrLetrecSyntax flag discriminates
         between the programmer and the implementation. */
        if (toPush instanceof Macro
            && !(toPush as Macro).isLetOrLetrecSyntax()) {
          throw Error.macro(name, 'bad syntax');
        }
        // TODO bl this doesn't seem like the right behavior. Investigate.
        args.push(toPush === null ? UNSPECIFIED_VALUE : toPush);
      } else if (cur instanceof Quote) {
        args.push(cur.getFirstChild()!);
      } else if (cur instanceof Lambda) {
        args.push(cur);
      } else if (cur instanceof Datum) {
        args.push(cur.clone(null /* parent */));
      } else {
        throw Error.internalInterpreterError('unexpected datum ' + cur);
      }
    }

    return args;
  }

  /**
   * Special logic for values and call-with-values. Example:
   *
   * (call-with-values (lambda () (values 1 2 3)) +)
   *
   * The "producer" procedure, (lambda () (values 1 2 3)), will desugar to
   * something like
   *
   * (values 1 2 3 [_0 ...])
   *
   * In this implementation, this will bind the JavaScript array [1, 2, 3] to _0.
   * Later on the trampoline, we reach (+ _0). We have to know that _0 refers
   * to an array of values, not a single value.
   */
  private evalArgsCallWithValues(): Value[]|null {
    if (this.firstOperand instanceof Identifier
        && !this.firstOperand.getNextSibling()) {
      const maybeArray = this.getEnv()!.get(this.firstOperand.getPayload());
      if (maybeArray instanceof Array) {
        return maybeArray;
      }
    }
    return null;
  }
}