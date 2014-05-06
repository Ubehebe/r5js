goog.provide('r5js.ast.Boolean');


goog.require('r5js.DatumType');
goog.require('r5js.ast.SimpleDatum');



/**
 * @param {boolean} val
 * @extends {r5js.ast.SimpleDatum.<boolean>}
 * @struct
 * @constructor
 */
r5js.ast.Boolean = function(val) {
  goog.base(this, val);
};
goog.inherits(r5js.ast.Boolean, r5js.ast.SimpleDatum);
