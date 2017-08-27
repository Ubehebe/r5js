goog.module('r5js.InMemoryInputPort');

const Character = goog.require('r5js.ast.Character');
const Datum = goog.require('r5js.Datum');
const InMemoryPortBuffer = goog.require('r5js.InMemoryPortBuffer');
const InputPort = goog.require('r5js.InputPort');
const Reader = goog.require('r5js.Reader');
const TokenStream = goog.require('r5js.TokenStream');

/** @implements {InputPort} */
class InMemoryInputPort {
    /** @param {!InMemoryPortBuffer} buffer */
    constructor(buffer) {
        /** @private */ this.closed_ = false;
        /** @const @private */ this.buffer_ = buffer;
        /** @private {?Datum} */ this.leftoverDatum_ = null;
    }

    /** @override */
    isCharReady() {
        return !this.buffer_.isEmpty();
    }

    /** @override */
    close() {
        this.closed_ = true;
    }

    /** @override */
    read() {
        const maybeDatum = this.readLeftoverDatum_();
        if (maybeDatum) {
            return maybeDatum;
        } else if (this.buffer_.isEmpty()) {
            return null;
        } else {
            const text = this.buffer_.getAndClear();
            this.leftoverDatum_ = Reader.forTokenStream(TokenStream.forText(text)).read();
            return this.read();
        }
    }

    /**
     * @return {?Datum}
     * @private
     */
    readLeftoverDatum_() {
        const retval = this.leftoverDatum_;
        if (retval) {
            this.leftoverDatum_ = this.leftoverDatum_.getNextSibling();
        }
        return retval;
    }

    /** @override */
    peekChar() {
        const c = this.buffer_.peekChar();
        return c ? new Character(c) : null;
    }

    /** @override */
    readChar() {
        const c = this.buffer_.getChar();
        return c ? new Character(c) : null;
    }
}

InputPort.addImplementation(InMemoryInputPort);

exports = InMemoryInputPort;