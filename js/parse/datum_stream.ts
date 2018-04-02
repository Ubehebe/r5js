import {Datum} from "../ast/datum";
import {CompoundDatum} from "../ast/compound_datum";

export class DatumStream {

  /**
   * The next datum to parse. When a parse of a node is successful, the next pointer advances to the
   * node's next sibling. Thus, next will only be null or undefined in two cases:
   *
   * 1. EOF
   * 2. Advancing past the end of a nonempty list.
   */
  private next: Datum | null;

  /**
   * The last datum parsed. We only need this in order to figure out where to go next after
   * finishing parsing a list. prev is only updated in two cases:
   *
   * 1. Moving from a parent (= this.prev) to its first child (= this.next)
   * 2. Moving from a sibling (= this.prev) to its next sibling (= this.next)
   *
   * Thus, prev is only null until the first successful move from parent to first child or from
   * sibling to next sibling, and is never thereafter null.
   */
  private prev: Datum | null = null;

  /** @param  root The root of the parse tree. */
  private constructor(root: Datum) {
    this.next = root;
  }

  /** @param root The root of the parse tree. */
  static create(root: Datum): DatumStream {
    return new DatumStream(root);
  }

  getNextDatum(): Datum | null {
    return this.next;
  }

  advanceTo(next: Datum) {
    this.next = next;
  }

  advanceToChild() {
    this.prev = this.next;
    this.next = (this.next as CompoundDatum).getFirstChild();
  }

  advanceToNextSibling() {
    this.prev = this.next;
    this.next = this.next!.getNextSibling();
  }

  /** @return True iff the advance worked. */
  maybeAdvanceToNextSiblingOfParent(): boolean {
    if (!this.next) {
      // We have fallen off the end of a non-empty list. For example, in
      //
      // (a b (c d) e)
      //
      // we have just finished parsing d. next is null, prev is d, prev.parent_ is (c d), and
      // prev.parent_.nextSibling_ is e, which is where we want to go next.
      this.next = this.prev!.getParent()
          && this.prev!.getParent()!.getNextSibling();
      return true;
    } else {
      // If we're not at the end of a list, this parse must fail.
      return false;
    }
  }
}