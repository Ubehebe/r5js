goog.provide('r5js.ast.CompoundDatum');


goog.require('r5js.Datum');



/**
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.ast.CompoundDatum = function() {
  goog.base(this);
};
goog.inherits(r5js.ast.CompoundDatum, r5js.Datum);


/** @return {r5js.Datum} */
r5js.ast.CompoundDatum.prototype.getFirstChild = function() {
  return this.firstChild_;
};


/** @param {!r5js.Datum} firstChild */
r5js.ast.CompoundDatum.prototype.setFirstChild = function(firstChild) {
  this.firstChild_ = firstChild;
};


/**
 * Clears the first child.
 * TODO bl is this necessary?
 */
r5js.ast.CompoundDatum.prototype.clearFirstChild = function() {
  this.firstChild_ = null;
};


/**
 * @param {function(this: T, !r5js.Datum)} callback
 * @param {T=} opt_context
 * @template T
 */
r5js.ast.CompoundDatum.prototype.forEachChild = function(
    callback, opt_context) {
  for (var cur = this.getFirstChild(); cur; cur = cur.getNextSibling()) {
    callback.call(opt_context, cur);
  }
};


/**
 * Example:
 *
 * `(a `(b ,(+ x y) ,(foo ,(+ z w) d) e) f)
 *
 * should be decorated as
 *
 * `1(a `2(b ,2(+ x y) ,2(foo ,1(+ z w) d) e) f)
 *
 * @param {number} qqLevel The level of quasiquotation.
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.ast.CompoundDatum.prototype.setQuasiquotationLevel = function(qqLevel) {
  this.forEachChild(function(child) {
    if (child instanceof r5js.ast.CompoundDatum) {
      child.setQuasiquotationLevel(qqLevel);
    }
  });
  return this;
};
