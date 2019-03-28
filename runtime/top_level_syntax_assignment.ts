import {Identifier} from "../ast/identifier";
import {ProcCallLike} from "../ast/proc_call_like";
import {SiblingBuffer} from "../ast/sibling_buffer";
import {TopLevelAssignment} from "./top_level_assignment";

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