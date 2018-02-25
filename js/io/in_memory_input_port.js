goog.module('r5js.InMemoryInputPort');

const {Character} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/character');
const {Datum} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {InputPort, addInputPortImpl} = require('/js/io/input_port_collect_es6_sources.es6/node_modules/__main__/js/io/input_port');
const Reader = goog.require('r5js.Reader');
const {TokenStream} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/token_stream');
const {InMemoryPortBuffer} = require('/js/io/in_memory_port_buffer_collect_es6_sources.es6/node_modules/__main__/js/io/in_memory_port_buffer');

class InMemoryInputPort extends InputPort {
    /** @param {!InMemoryPortBuffer} buffer */
    constructor(buffer) {
        super();
        /** @const @private */ this.buffer_ = buffer;
        /** @private {?Datum} */ this.leftoverDatum_ = null;
    }

    /** @override */
    isCharReady() {
        return !this.buffer_.isEmpty();
    }

    /** @override */
    close() {
        // TODO: implement?
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

addInputPortImpl(InMemoryInputPort);

exports = InMemoryInputPort;