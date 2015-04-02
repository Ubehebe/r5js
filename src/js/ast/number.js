goog.provide('r5js.ast.Number');


goog.require('r5js.ast.SimpleDatum');



r5js.ast.Number = /** @extends {r5js.ast.SimpleDatum<number>} */ class extends r5js.ast.SimpleDatum {
 /** @param {number} x */
  constructor(x) {
   super(x);
  }
};
