goog.provide('r5js.ast.Vector');


goog.require('r5js.Datum');
goog.require('r5js.ast.CompoundDatum');



/**
 * @param {!r5js.Datum|!Array.<!r5js.Datum>} firstChildOrArray
 * @extends {r5js.ast.CompoundDatum}
 * @struct
 * @constructor
 * TODO bl: this class illustrates the interpreter's confusion of abstract
 * syntax, intermediate representation, and runtime representation.
 * Having Scheme vectors be backed by JavaScript arrays is a reasonable
 * runtime decision, but is independent of AST and IR decisions.
 * This constructor cannot call {@link #convertVectorToArrayBacked_}
 * on its argument; that changes the structure of the syntax tree,
 * which affects parsing. Only the runtime primitive procedures can do this.
 */
r5js.ast.Vector = function(firstChildOrArray) {
  goog.base(this);

  /** @private */ this.arrayBacked_ = goog.isArray(firstChildOrArray);

  /** @const @private {!Array.<!r5js.Datum>} */ this.array_ =
      this.arrayBacked_ ?
      /** @type {!Array.<!r5js.Datum>} */ (firstChildOrArray) :
      [];

  if (firstChildOrArray instanceof r5js.Datum) {
    this.setFirstChild(firstChildOrArray);
  }
};
goog.inherits(r5js.ast.Vector, r5js.ast.CompoundDatum);


/** @return {number} */
r5js.ast.Vector.prototype.vectorLength = function() {
  if (!this.arrayBacked_) {
    this.convertVectorToArrayBacked_();
  }
  return this.array_.length;
};


/**
 * @param {number} index
 * @return {!r5js.Datum}
 */
r5js.ast.Vector.prototype.vectorRef = function(index) {
  if (!this.arrayBacked_) {
    this.convertVectorToArrayBacked_();
  }
  return this.array_[index];
};


/**
 * @param {number} index
 * @param {!r5js.Datum} val
 */
r5js.ast.Vector.prototype.vectorSet = function(index, val) {
  if (!this.arrayBacked_) {
    this.convertVectorToArrayBacked_();
  }
  this.array_[index] = val;
};


/**
 * Vector literals are constructed by the reader as linked lists
 * with no random access, while vectors created programmatically
 * via make-vector can just use JavaScript arrays. Instead of building
 * logic into the reader to convert its inefficient vectors to array-backed
 * ones, we check in every primitive vector procedure if the vector
 * is array-backed, and mutate it in place if it isn't. There may
 * be bugs involving the lost child/sibling pointers.
 * @private
 */
r5js.ast.Vector.prototype.convertVectorToArrayBacked_ = function() {
  this.forEachChild(function(child) {
    this.array_.push(child);
  }, this);
  this.clearFirstChild();
  this.arrayBacked_ = true;
};
