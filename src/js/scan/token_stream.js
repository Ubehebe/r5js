goog.provide('r5js.scan.TokenStream');
goog.provide('r5js.scan.TokenStream.Checkpoint');



/**
 * Abstraction for a stream of tokens that can be checkpointed and restored
 * when parsing fails. (This interface exists to avoid having to build
 * checkpoint and restore logic into the scanner itself.)
 * @interface
 * @extends {r5js.IScanner}
 */
r5js.scan.TokenStream = function() {};


/** @typedef {number} */
r5js.scan.TokenStream.Checkpoint;


/**
 * Establishes a checkpoint that can be restored by
 * {@link r5js.scan.TokenStream#restore}.
 * @return {!r5js.scan.TokenStream.Checkpoint}
 */
r5js.scan.TokenStream.prototype.checkpoint = function() {};


/**
 * Restores the state of the token stream represented by checkpoint.
 * @param {!r5js.scan.TokenStream.Checkpoint} checkpoint
 */
r5js.scan.TokenStream.prototype.restore = function(checkpoint) {};


