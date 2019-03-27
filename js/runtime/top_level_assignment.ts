import {Identifier} from "../ast/identifier";
import {ProcCallLike} from "../ast/proc_call_like";
import {SiblingBuffer} from "../ast/sibling_buffer";
import {Value} from "../value";
import {Assignment} from "./assignment";

export class TopLevelAssignment extends Assignment {
  constructor(firstOperand: Identifier) {
    super(firstOperand);
  }

  /** @override */
  mutateEnv(name: string, val: Value) {
    this.getEnv()!.mutate(name, val, true /* isTopLevel */);
  }

  static of(dstName: string, srcName: string): ProcCallLike {
    const operands = new SiblingBuffer()
        .appendSibling(new Identifier(dstName))
        .appendSibling(new Identifier(srcName))
        .toSiblings();
    return new TopLevelAssignment(operands as Identifier);
  }
}