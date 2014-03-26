goog.provide('r5js.ast.UnquoteSplicing');


goog.require('r5js.Datum');
goog.require('r5js.parse.Terminals');



/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.ast.UnquoteSplicing = function(firstChild) {
  goog.base(this);
  this.setType(r5js.parse.Terminals.COMMA_AT);
  if (firstChild) {
    this.setFirstChild(firstChild);
  }
};
goog.inherits(r5js.ast.UnquoteSplicing, r5js.Datum);


/** @override */
r5js.ast.UnquoteSplicing.prototype.stringForOutputMode = function(outputMode) {
  var children = this.mapChildren(function(child) {
    return child.stringForOutputMode(outputMode);
  });
  return r5js.parse.Terminals.COMMA_AT + children.join(' ');
};


/** @override */
r5js.ast.UnquoteSplicing.prototype.setQuasiquotationLevel = function(qqLevel) {
  this.qqLevel = qqLevel;
  return goog.base(this, 'setQuasiquotationLevel', qqLevel - 1);
};
