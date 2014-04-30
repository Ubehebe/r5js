goog.provide('r5js.ast.Quote');


goog.require('r5js.Pair');
goog.require('r5js.ast.CompoundDatum');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.List');
goog.require('r5js.parse.Terminals');
goog.require('r5js.runtime.UNSPECIFIED_VALUE');



/**
 * @param {r5js.Datum} firstChild
 * @implements {r5js.Pair}
 * @extends {r5js.ast.CompoundDatum}
 * @struct
 * @constructor
 */
r5js.ast.Quote = function(firstChild) {
  goog.base(this);
  if (firstChild) {
    this.setFirstChild(firstChild.setImmutable());
  }
};
goog.inherits(r5js.ast.Quote, r5js.ast.CompoundDatum);
r5js.Pair.addImplementation(r5js.ast.Quote);


/** @const @private {!r5js.runtime.Value} */
r5js.ast.Quote.CAR_ = new r5js.ast.Identifier(r5js.parse.Terminals.QUOTE);


/** @override */
r5js.ast.Quote.prototype.car = function() {
  return r5js.ast.Quote.CAR_;
};


/** @override */
r5js.ast.Quote.prototype.cdr = function() {
  return new r5js.ast.List(this.getFirstChild());
};


/** @override */
r5js.ast.Quote.prototype.stringForOutputMode = function(outputMode) {
  var children = this.mapChildren(function(child) {
    return child.stringForOutputMode(outputMode);
  });
  return r5js.parse.Terminals.TICK + children.join(' ');
};


/** @override */
r5js.ast.Quote.prototype.fixParserSensitiveIds = goog.nullFunction;
