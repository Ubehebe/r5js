goog.provide('r5js.TokenStream');



/** @interface */
r5js.TokenStream = function() {};


/** @return {r5js.Token} The next token, or null if there are no more. */
r5js.TokenStream.prototype.nextToken = function() {};


/** @typedef {number} */
r5js.TokenStream.Checkpoint;


/**
 * Establishes a checkpoint that can be restored by
 * {@link r5js.TokenStream#restore}.
 * @return {!r5js.TokenStream.Checkpoint}
 */
r5js.TokenStream.prototype.checkpoint = function() {};


/**
 * Restores the state of the token stream represented by checkpoint.
 * @param {!r5js.TokenStream.Checkpoint} checkpoint
 */
r5js.TokenStream.prototype.restore = function(checkpoint) {};
