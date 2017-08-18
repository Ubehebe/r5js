goog.module('r5js.Evaluator');

const Promise = goog.require('goog.Promise');

/**
 * This is the most important interface in the whole codebase,
 * the main abstraction used by clients to evaluate Scheme source code.
 * Its methods are asynchronous, returning Promises, because
 * evaluation should not block the main thread (for example, the browser UI).
 * @interface
 */
class Evaluator {
 /**
  * @param {string} input Input to evaluate.
  * @return {!Promise<string>}  If evaluation succeeds,
  * this promise will be resolved with a string representation of the Scheme
  * value (as if it was serialized with the {@code write} procedure).
  * If evaluation fails, the promise will be rejected with an {@link r5js.Error}
  * explaining what went wrong.
  */
 evaluate(input) {}
}

exports = Evaluator;