goog.module('r5js.ast.Number');
goog.module.declareLegacyNamespace();

const SimpleDatum = goog.require('r5js.ast.SimpleDatum');

/** @extends {SimpleDatum<number>} */
class Number extends SimpleDatum {
 /** @param {number} x */
  constructor(x) {
   super(x);
  }
}

exports = Number;
