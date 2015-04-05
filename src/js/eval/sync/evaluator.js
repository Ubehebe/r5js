goog.module('r5js.sync.Evaluator');

const error = goog.require('r5js.Error');

/** @interface */
class Evaluator {
 /**
  * @param {string} input
  * @return {string}
  * @throws {!Error}
  */
 evaluate(input) {}
}

exports = Evaluator;