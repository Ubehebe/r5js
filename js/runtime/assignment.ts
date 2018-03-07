import {ProcCallLike, ProcCallResult, UNSPECIFIED_VALUE} from "../ast/datum";
import {Identifier} from "../ast/identifier";
import {Macro} from "../eval/macro"; // TODO should be macro/macro
import {Error} from "../error";
import {SimpleDatum} from "../ast/simple_datum";
import {SiblingBuffer} from "../ast/sibling_buffer";
import {Parser} from "../eval/parser"; // TODO

export class Assignment extends ProcCallLike {

  constructor(private readonly firstOperand: Identifier) {
    super();
  }

  /** @override */
  evalAndAdvance(
      resultStruct: ProcCallResult,
      envBuffer: IEnvironment,
      parserProvider: (Datum) => Parser) {
    const src = this.getEnv()!.get(
        (this.firstOperand.getNextSibling() as SimpleDatum<any>).getPayload())!;
    this.checkForImproperSyntaxAssignment(src);
    this.mutateEnv(this.firstOperand.getPayload(), src);
    // R5RS 4.1.6: the value of an assignment is unspecified.
    resultStruct.setValue(UNSPECIFIED_VALUE);
    this.bindResult(UNSPECIFIED_VALUE);
    const nextContinuable = this.getNext();
    if (nextContinuable) {
      resultStruct.setNext(nextContinuable);
    }
  }

  /**
   * In Scheme, macros can be bound to identifiers but they are not really
   * first-class citizens; you cannot say (define x let) because the text "let"
   * does not parse as an expression (at least if it has its normal binding).
   * In this implementation, however, macros are objects that go into and come
   * out of environments like any other kind of objects. All kinds of assignments
   * -- top-level, internal, syntax, non-syntax -- go through this method,
   * so we have to make sure we don't accidentally permit some illegal behavior.
   *
   * If we're trying to assign a {@link r5js.Macro} here, the programmer is
   * requesting this assignment and we ought to signal an error.
   *
   * @see {r5js.TopLevelSyntaxAssignment#checkForImproperSyntaxAssignment},
   * which bypasses this check.
   */
  protected checkForImproperSyntaxAssignment(val: Value) {
    if (val instanceof Macro && !(val as Macro).isLetOrLetrecSyntax()) {
      throw Error.internalInterpreterError('TODO bl');
    }
  }

  protected mutateEnv(name: string, val: Value) {
    this.getEnv()!.mutate(name, val, false /* isTopLevel */);
  }

  static create(dstName: string, srcName: string): ProcCallLike {
    const operands = new SiblingBuffer()
        .appendSibling(new Identifier(dstName))
        .appendSibling(new Identifier(srcName))
        .toSiblings() as Identifier;
    return new Assignment(operands);
  }
}