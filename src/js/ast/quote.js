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
  return r5js.parse.Terminals.TICK + children.join(' ');
};


/** @override */
r5js.ast.Quote.prototype.fixParserSensitiveIds = goog.nullFunction;
