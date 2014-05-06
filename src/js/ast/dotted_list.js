goog.provide('r5js.ast.DottedList');


goog.require('r5js.CdrHelper');
goog.require('r5js.Pair');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.CompoundDatum');
goog.require('r5js.parse.Terminals');



/**
 * @param {r5js.Datum} firstChild
 * @implements {r5js.Pair}
 * @extends {r5js.ast.CompoundDatum}
 * @struct
 * @constructor
 */
r5js.ast.DottedList = function(firstChild) {
  goog.base(this);
  if (firstChild) {
    this.setFirstChild(firstChild);
  }
};
goog.inherits(r5js.ast.DottedList, r5js.ast.CompoundDatum);
r5js.Pair.addImplementation(r5js.ast.DottedList);


/** @override */
r5js.ast.DottedList.prototype.isImproperList = function() { return true; };


/** @override */
r5js.ast.DottedList.prototype.car = function() {
  return /** @type {!r5js.Datum} */ (this.getFirstChild());
};


/** @override */
r5js.ast.DottedList.prototype.cdr = function() {
  var startOfCdr = this.getFirstChild().getNextSibling();
  var ans;
  if (startOfCdr) {
    if (startOfCdr.getNextSibling()) {
      ans = new r5js.SiblingBuffer().
                appendSibling(startOfCdr).
                toList(r5js.ast.DottedList);
    } else {
      ans = startOfCdr;
    }
    if (ans instanceof r5js.ast.CompoundDatum) {
      ans.setCdrHelper(new r5js.CdrHelper(this, startOfCdr));
    }
    return ans;
  } else {
    return new r5js.SiblingBuffer().toList(r5js.ast.List);
  }
};


///** @override */
//r5js.ast.DottedList.prototype.stringForOutputMode = function(outputMode) {
//  var children = this.mapChildren(function(child) {
//    return child.stringForOutputMode(outputMode);
//  });
//  Insert the dot at the next-to-last location.
//  children.splice(-1, 0, r5js.parse.Terminals.DOT);
//  return r5js.parse.Terminals.LPAREN +
//      children.join(' ') +
//      r5js.parse.Terminals.RPAREN;
//};
