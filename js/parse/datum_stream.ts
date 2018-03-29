import {Datum} from "../ast/datum";
import {CompoundDatum} from "../ast/compound_datum";

export class DatumStream {

  /**
   * The next datum to parse. When a parse of a node is successful, the next pointer advances to the
   * node's next sibling. Thus, next will only be null or undefined in two cases:
   *
   * 1. EOF
   * 2. Advancing past the end of a nonempty list. (The empty-list corner case is handled by
   * {@link EMPTY_LIST_SENTINEL}.)
   */
  private next: Datum | Object | null;

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
  private prev: Datum | Object | null = null;

  /** @param  root The root of the parse tree. */
  private constructor(root: Datum) {
    this.next = root;
  }

  /** @param root The root of the parse tree. */
  static create(root: Datum): DatumStream {
    return new DatumStream(root);
  }

  getNextDatum(): Datum | null {
    return this.next === EMPTY_LIST_SENTINEL
        ? null
        : this.next as Datum;
  }

  advanceTo(next: Datum) {
    this.next = next;
  }

  advanceToChild() {
    this.prev = this.next;
    // See comments in body of Parser() for explanation of emptyListSentinel.
    this.next = (this.next as CompoundDatum).getFirstChild() || EMPTY_LIST_SENTINEL;
  }

  advanceToNextSibling() {
    this.prev = this.next;
    this.next = (this.next as Datum).getNextSibling();
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
      this.next = (this.prev as Datum).getParent()
          && (this.prev as Datum).getParent()!.getNextSibling();
      return true;
    } else if (this.next === EMPTY_LIST_SENTINEL) {
      // We have fallen off the "end" of an empty list. For example, in
      //
      // (a b () e)
      //
      // we have just finished parsing (). next is emptyListSentinel, prev is (), and
      // prev.nextSibling_ is e, which is where we want to go next.
      this.next = (this.prev as Datum).getNextSibling();
      return true;
    } else {
      // If we're not at the end of a list, this parse must fail.
      return false;
    }
  }
}

/**
 * We use a special sentinel object to handle the corner case of an empty list. According to the
 * tree constructed by the reader (the structure of which the parser does not modify), an empty list
 * is simply a datum of type '(' whose firstSibling is null or undefined. This presents a problem
 * for the parser: when this.next is null, have we advanced past the end of a list, or was the list
 * empty to begin with? We must distinguish these cases, because they affect what to parse next.
 *
 * For a long time, I tried to distinguish them via some pointer trickery, but this concealed some
 * very subtle bugs. So I decided it was clearer to compare against a dedicated sentinel object.
 *
 * The sentinel is an immutable object with no state; we use it only for direct identity
 * comparisons. It is used only internally by the parser; it never enters the parse tree.
 */
const EMPTY_LIST_SENTINEL = new Object();