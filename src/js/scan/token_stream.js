goog.provide('r5js.TokenStream');



r5js.TokenStream = /** @interface */ class {
 /** @return {r5js.Token} The next token, or null if there are no more. */
 nextToken() {}

 /**
  * Establishes a checkpoint that can be restored by
  * {@link r5js.TokenStream#restore}.
  * @return {!r5js.TokenStream.Checkpoint}
  */
 checkpoint() {}

 /**
  * Restores the state of the token stream represented by checkpoint.
  * @param {!r5js.TokenStream.Checkpoint} checkpoint
  */
 restore(checkpoint) {}
};


/** @typedef {number} */
r5js.TokenStream.Checkpoint;