goog.provide('r5js.ast.Vector');


goog.require('r5js.Datum');
goog.require('r5js.DatumType');



/**
 * @param {!Array.<?>} array TODO bl narrow generic type.
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.ast.Vector = function(array) {
  goog.base(this);
  this.setPayload(array);
  this.setType(r5js.DatumType.VECTOR); // TODO bl remove
};
goog.inherits(r5js.ast.Vector, r5js.Datum);


/** @override */
r5js.ast.Vector.prototype.stringForOutputMode = function(outputMode) {
  if (this.isArrayBacked()) {
    var ans = '#(';
    if (this.getPayload().length > 0) {
      for (var i = 0; i < this.getPayload().length - 1; ++i)
        ans += this.getPayload()[i] + ' ';
      ans += this.getPayload()[this.getPayload().length - 1];
    }
    return ans + ')';
  }
  // fallthrough for non-array-backed vectors
  var children = this.mapChildren(function(child) {
    return child.stringForOutputMode(outputMode);
  });
  // Insert the dot at the next-to-last location.
  children.splice(-1, 0, r5js.parse.Terminals.DOT);
  return r5js.parse.Terminals.LPAREN +
      children.join(' ') +
      r5js.parse.Terminals.RPAREN;
};
