goog.provide('r5js.ast.Number');


goog.require('r5js.DatumType');
goog.require('r5js.ast.SimpleDatum');



/**
 * @param {number} x
 * @extends {r5js.ast.SimpleDatum}
 * @struct
 * @constructor
 */
r5js.ast.Number = function(x) {
  goog.base(this);
  this.setType(r5js.DatumType.NUMBER); // TODO bl remove
  this.setPayload(x);
};
goog.inherits(r5js.ast.Number, r5js.ast.SimpleDatum);


/** @override */
r5js.ast.Number.prototype.stringForOutputMode = function(outputMode) {
  return this.getPayload() + '';
};