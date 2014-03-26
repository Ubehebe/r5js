goog.provide('r5js.DottedList');


goog.require('r5js.Datum');
goog.require('r5js.parse.Terminals');



/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.DottedList = function(firstChild) {
  goog.base(this);
  this.setType(r5js.parse.Terminals.LPAREN_DOT); // TODO bl remove
  if (firstChild) {
    this.setFirstChild(firstChild);
  }
};
goog.inherits(r5js.DottedList, r5js.Datum);


/** @override */
r5js.DottedList.prototype.stringForOutputMode = function(outputMode) {
  var children = this.mapChildren(function(child) {
    return child.stringForOutputMode(outputMode);
  });
  // Insert the dot at the next-to-last location.
  children.splice(-1, 0, r5js.parse.Terminals.DOT);
  return r5js.parse.Terminals.LPAREN +
      children.join(' ') +
      r5js.parse.Terminals.RPAREN;
};
