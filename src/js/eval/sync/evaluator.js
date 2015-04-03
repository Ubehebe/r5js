goog.module('r5js.sync.Evaluator');

/** @interface */
class Evaluator {
 /**
  * @param {string} input
  * @return {string}
  * @throws {!r5js.Error}
  */
 evaluate(input) {}
}

exports = Evaluator;