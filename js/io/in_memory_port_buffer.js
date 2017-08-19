goog.module('r5js.InMemoryPortBuffer');


class InMemoryPortBuffer {
    constructor() {
        /** @private */ this.buffer_ = '';
    }

    /** @return {boolean} */
    isEmpty() {
        return !this.buffer_.length;
    }

    /** @return {?string} */
    getChar() {
        const c = this.peekChar();
        if (c) {
            this.buffer_ = this.buffer_.substr(1);
        }
        return c;
    }

    /** @return {?string} */
    peekChar() {
        return this.buffer_.length ? this.buffer_.charAt(0) : null;
    }

    /** @return {string} */
    getAndClear() {
        const retval = this.buffer_;
        this.buffer_ = '';
        return retval;
    }

    /** @param {string} str */
    append(str) {
        this.buffer_ += str;
    }
}

exports = InMemoryPortBuffer;