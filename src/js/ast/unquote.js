goog.provide('r5js.ast.Unquote');


goog.require('r5js.Datum');
goog.require('r5js.parse.Terminals');



/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.ast.Unquote = function(firstChild) {
  goog.base(this);
  if (firstChild) {
    this.setFirstChild(firstChild);
  }
};
goog.inherits(r5js.ast.Unquote, r5js.Datum);


/** @override */
r5js.ast.Unquote.prototype.stringForOutputMode = function(outputMode) {
  var children = this.mapChildren(function(child) {
    return child.stringForOutputMode(outputMode);
  });
  return r5js.parse.Terminals.COMMA + children.join(' ');
};


/** @override */
r5js.ast.Unquote.prototype.setQuasiquotationLevel = function(qqLevel) {
  this.qqLevel = qqLevel;
  return goog.base(this, 'setQuasiquotationLevel', qqLevel - 1);
};
