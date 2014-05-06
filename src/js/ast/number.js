goog.provide('r5js.ast.Number');


goog.require('r5js.DatumType');
goog.require('r5js.ast.SimpleDatum');



/**
 * @param {number} x
 * @extends {r5js.ast.SimpleDatum.<number>}
 * @struct
 * @constructor
 */
r5js.ast.Number = function(x) {
  goog.base(this, x);
};
goog.inherits(r5js.ast.Number, r5js.ast.SimpleDatum);
