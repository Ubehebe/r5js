goog.provide('r5js.ast.Quote');


goog.require('r5js.ast.Literal');
goog.require('r5js.parse.Terminals');



/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.ast.Literal}
 * @struct
 * @constructor
 */
r5js.ast.Quote = function(firstChild) {
  goog.base(this);
  this.setType(r5js.parse.Terminals.TICK); // TODO bl remove
  if (firstChild) {
    this.setFirstChild(firstChild.setImmutable());
  }
};
goog.inherits(r5js.ast.Quote, r5js.ast.Literal);


/** @override */
r5js.ast.Quote.prototype.stringForOutputMode = function(outputMode) {
  var children = this.mapChildren(function(child) {
    return child.stringForOutputMode(outputMode);
  });
  return this.getType() + children.join(' ');
};


/** @override */
r5js.ast.Quote.prototype.fixParserSensitiveIds = goog.nullFunction;
