goog.module('r5js.Terminal');

const Promise = goog.require('goog.Promise');

/**
 * "Dumb" terminal that {@link r5js.Repl} can read to and write from.
 * In contrast to a Repl, a Terminal knows nothing about Scheme.
 * @interface
 */
class Terminal {
 /** @return {!Promise<string>} The next line of input. */
 getNextLineOfInput() {}

 /** @param {string} str String to print. */
 print(str) {}

 /** @param {string} str Error message to print. */
 error(str) {}
}

exports = Terminal;
