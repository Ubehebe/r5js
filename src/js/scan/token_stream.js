goog.module('r5js.TokenStream');

const Token = goog.require('r5js.Token');

/** @interface */
class TokenStream {
    /** @return {Token} The next token, or null if there are no more. */
    nextToken() {}

    /**
     * Establishes a checkpoint that can be restored by {@link restore}.
     * @return {!TokenStream.Checkpoint}
     */
    checkpoint() {}

    /**
     * Restores the state of the token stream represented by {@code checkpoint}.
     * @param {!TokenStream.Checkpoint} checkpoint
     */
    restore(checkpoint) {
    }
}

/** @typedef {number} */
TokenStream.Checkpoint;

exports = TokenStream;