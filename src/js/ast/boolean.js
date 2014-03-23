goog.provide('r5js.ast.Boolean');


goog.require('r5js.DatumType');
goog.require('r5js.ast.SimpleDatum');



/**
 * @param {boolean} val
 * @extends {r5js.ast.SimpleDatum}
 * @struct
 * @constructor
 */
r5js.ast.Boolean = function(val) {
  goog.base(this);
  this.setPayload(val);
  this.setType(r5js.DatumType.BOOLEAN); // TODO bl remove
};
goog.inherits(r5js.ast.Boolean, r5js.ast.SimpleDatum);


/** @override */
r5js.ast.Boolean.prototype.stringForOutputMode = function(outputMode) {
  return this.getPayload() ?
      r5js.ast.Boolean.TRUE_STRING_ :
      r5js.ast.Boolean.FALSE_STRING_;
};


/** @private @const */ r5js.ast.Boolean.TRUE_STRING_ = '#t';
/** @private @const */ r5js.ast.Boolean.FALSE_STRING_ = '#f';
