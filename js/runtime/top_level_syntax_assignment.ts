import {TopLevelAssignment} from "./top_level_assignment";
import {ProcCallLike} from "../ast/proc_call_like";
import {Identifier} from "../ast/identifier";
import {SiblingBuffer} from "../ast/sibling_buffer";

export class TopLevelSyntaxAssignment extends TopLevelAssignment {
  constructor(firstOperand: Identifier) {
    super(firstOperand);
  }

  /** @override */
  checkForImproperSyntaxAssignment() {
  }

  static of(dstName: string, srcName: string): ProcCallLike {
    const operands = new SiblingBuffer()
        .appendSibling(new Identifier(dstName))
        .appendSibling(new Identifier(srcName))
        .toSiblings();
    return new TopLevelSyntaxAssignment(operands as Identifier);
  }
}