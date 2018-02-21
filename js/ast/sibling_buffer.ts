import {Datum} from './datum';

/**
 * Just a buffer to accumulate siblings without the client having to do
 * the pointer arithmetic.
 */
export class SiblingBuffer {

  private first_: Datum|null = null;
  private last_: Datum|null = null;

    isEmpty(): boolean {
        return !this.first_;
    }

    appendSibling(node: Datum): SiblingBuffer /* for chaining. TODO use polymorphic `this` */ {
        if (!this.first_) {
            this.first_ = node;
            this.last_ = node.lastSibling();
        } else {
            this.last_!.setNextSibling(node);
            this.last_ = node.lastSibling();
        }
        return this;
    }

    toSiblings(): Datum|null {
        return this.first_;
    }

    toList<T extends Datum>(ctor: new (datum: Datum) => T): T {
        const ans = new ctor(this.first_!);
        if (this.last_ && ans) {
            this.last_.setParent(ans);
        }
        return ans;
    }
}