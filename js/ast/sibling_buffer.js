goog.module('r5js.SiblingBuffer');

const Datum = goog.require('r5js.Datum');

/**
 * Just a buffer to accumulate siblings without the client having to do
 * the pointer arithmetic.
 */
class SiblingBuffer {
   constructor() {
        /** @private {?Datum} */ this.first_ = null;
        /** @private {?Datum} */ this.last_ = null;
    }

    /** @return {boolean} True iff the buffer is empty. */
    isEmpty() {
        return !this.first_;
    }

    /**
     * @param {!Datum} node Node to append.
     * @return {!SiblingBuffer} This object, for chaining.
     */
    appendSibling(node) {
        if (!this.first_) {
            this.first_ = node;
            this.last_ = node.lastSibling();
        } else {
            this.last_.setNextSibling(node);
            this.last_ = node.lastSibling();
        }
        return this;
    }

    /** @return {?Datum} */
    toSiblings() {
        return this.first_;
    }

    /**
     * @param {function(new: T, !Datum)} ctor Constructor to use for the returned list.
     * @return {T}
     * @template T
     */
    toList(ctor) {
        const ans = new ctor(/** @type {!Datum} */ (this.first_));
        if (this.last_ && ans) {
            this.last_.setParent(ans);
        }
        return ans;
    }
}

exports = SiblingBuffer;
