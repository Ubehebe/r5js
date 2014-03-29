goog.provide('r5js.ast.Vector');


goog.require('r5js.Datum');
goog.require('r5js.DatumType');



/**
 * @param {!r5js.Datum|!Array.<!r5js.Datum>} firstChildOrArray
 * @extends {r5js.Datum}
 * @struct
 * @constructor
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
  if (this.isArrayBacked_()) {
    var ans = '#(';
    if (this.getPayload().length > 0) {
      for (var i = 0; i < this.getPayload().length - 1; ++i)
        ans += this.getPayload()[i] + ' ';
      ans += this.getPayload()[this.getPayload().length - 1];
    }
    return ans + ')';
  }
  // fallthrough for non-array-backed vectors
  var children = this.mapChildren(function(child) {
    return child.stringForOutputMode(outputMode);
  });
  // Insert the dot at the next-to-last location.
  children.splice(-1, 0, r5js.parse.Terminals.DOT);
  return r5js.parse.Terminals.LPAREN +
      children.join(' ') +
      r5js.parse.Terminals.RPAREN;
};


/**
 * @override
 * TODO bl explain or remove.
 */
r5js.ast.Vector.prototype.unwrap = function() {
  return this;
};
