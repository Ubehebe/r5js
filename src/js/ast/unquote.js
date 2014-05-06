goog.provide('r5js.ast.Unquote');


goog.require('r5js.ast.CompoundDatum');
goog.require('r5js.parse.Terminals');



/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.ast.CompoundDatum}
 * @struct
 * @constructor
 */
r5js.ast.Unquote = function(firstChild) {
  goog.base(this);
  if (firstChild) {
    this.setFirstChild(firstChild);
  }
};
goog.inherits(r5js.ast.Unquote, r5js.ast.CompoundDatum);


/** @override */
r5js.ast.Unquote.prototype.setQuasiquotationLevel = function(qqLevel) {
  this.qqLevel = qqLevel;
  return goog.base(this, 'setQuasiquotationLevel', qqLevel - 1);
};
