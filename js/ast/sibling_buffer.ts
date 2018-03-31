import {Datum} from './datum';

/**
 * Just a buffer to accumulate siblings without the client having to do
 * the pointer arithmetic.
 */
export class SiblingBuffer {

  private first: Datum|null = null;
  private last: Datum|null = null;

    isEmpty(): boolean {
        return !this.first;
    }

    appendSibling(node: Datum): SiblingBuffer /* for chaining. TODO use polymorphic `this` */ {
        if (!this.first) {
            this.first = node;
            this.last = node.lastSibling();
        } else {
            this.last!.setNextSibling(node);
            this.last = node.lastSibling();
        }
        return this;
    }

    toSiblings(): Datum|null {
        return this.first;
    }

    toList<T extends Datum>(ctor: new (datum: Datum) => T): T {
        const ans = new ctor(this.first!);
        if (this.last && ans) {
            this.last.setParent(ans);
        }
        return ans;
    }
}