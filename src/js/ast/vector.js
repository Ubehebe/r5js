goog.provide('r5js.ast.Vector');


goog.require('r5js.Datum');
goog.require('r5js.DatumType');



/**
 * @param {!r5js.Datum|!Array.<!r5js.Datum>} firstChildOrArray
 * @extends {r5js.Datum}
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
  if (goog.isArray(firstChildOrArray)) {
    this.setPayload(firstChildOrArray);
  } else {
    this.setFirstChild(firstChildOrArray);
  }
};
goog.inherits(r5js.ast.Vector, r5js.Datum);


/** @return {number} */
r5js.ast.Vector.prototype.vectorLength = function() {
  if (!this.isArrayBacked_()) {
    this.convertVectorToArrayBacked_();
  }
  return this.getPayload().length;
};


/**
 * @param {number} index
 * @return {!r5js.Datum}
 */
r5js.ast.Vector.prototype.vectorRef = function(index) {
  if (!this.isArrayBacked_()) {
    this.convertVectorToArrayBacked_();
  }
  return this.getPayload()[index];
};


/**
 * @param {number} index
 * @param {!r5js.Datum} val
 */
r5js.ast.Vector.prototype.vectorSet = function(index, val) {
  if (!this.isArrayBacked_()) {
    this.convertVectorToArrayBacked_();
  }
  this.getPayload()[index] = val;
};


/**
 * @return {boolean} True iff this datum represents a vector
 * and is backed by a JavaScript array.
 * See {@link r5js.Datum.convertVectorToArrayBacked_}.
 * TODO bl: this method doesn't actually check that the datum represents
 * a vector.
 * @private
 */
r5js.ast.Vector.prototype.isArrayBacked_ = function() {
  return !!this.getPayload();
};


/**
 * Vector literals are constructed by the reader as linked lists
 * with no random access, while vectors created programmatically
 * via make-vector can just use JavaScript arrays. Instead of building
 * logic into the reader to convert its inefficient vectors to array-backed
 * ones, we check in every primitive vector procedure if the vector
 * is array-backed, and mutate it in place if it isn't. There may
 * be bugs involving the lost child/sibling pointers.
 * @return {!r5js.Datum} This object, for chaining.
 * @private
 * @suppress {checkTypes} for setFirstChild(null). TODO bl remove.
 */
r5js.ast.Vector.prototype.convertVectorToArrayBacked_ = function() {
  var newPayload = [];
  this.forEachChild(function(child) {
    newPayload.push(child);
  });
  this.setPayload(newPayload);
  this.setFirstChild(null);
  return this;
};


/** @override */
r5js.ast.Vector.prototype.stringForOutputMode = function(outputMode) {
  var childStrings = this.isArrayBacked_() ?
      this.getPayload().map(function(datum) {
        return datum.stringForOutputMode(outputMode);
          }) :
      this.mapChildren(function(child) {
        return child.stringForOutputMode(outputMode);
      });

  return r5js.parse.Terminals.LPAREN_VECTOR +
      childStrings.join(' ') +
      r5js.parse.Terminals.RPAREN;
};


/**
 * @override
 * TODO bl explain or remove.
 */
r5js.ast.Vector.prototype.unwrap = function() {
  return this;
};
