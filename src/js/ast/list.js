goog.provide('r5js.ast.List');


goog.require('r5js.Datum');
goog.require('r5js.parse.Terminals');



/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.ast.List = function(firstChild) {
  goog.base(this);
  this.setType(r5js.parse.Terminals.LPAREN); // TODO bl remove
  if (firstChild) {
    this.setFirstChild(firstChild);
  }

  /** @private */ this.dirty_ = false;
};
goog.inherits(r5js.ast.List, r5js.Datum);


/** Marks dirty. */
r5js.ast.List.prototype.markDirty = function() {
  this.dirty_ = true;
};


/** @return {boolean} */
r5js.ast.List.prototype.isDirty = function() {
  return this.dirty_;
};


/** @override */
r5js.ast.List.prototype.stringForOutputMode = function(outputMode) {
  /* Note: this will be an infinite loop for cyclical data
     structures created by the programmer through set-cdr!, etc.
     Some implementations do nice things, like print "holes" where
     a cycle starts. But the R5RS standard does not seem to define
     external representations for lists (vectors, etc.) that contain
     cycles. In general, the spirit of the standard seems to be that
     the programmer is responsible for mayhem caused by the creation
     of such structures.

     There is one exception: list? (a library procedure) must return
     false for cyclical lists. Accordingly, I've written the
     cycle-detecting logic wholly in Scheme, not bothering
     to reimplement it here. */
  var children = this.mapChildren(function(child) {
    return child.stringForOutputMode(outputMode);
  });
  return this.getType() + children.join(' ') + ')';
};
