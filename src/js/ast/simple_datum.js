goog.provide('r5js.ast.SimpleDatum');


goog.require('r5js.ast.Literal');



/**
 * @param {T} payload
 * @extends {r5js.ast.Literal}
 * @struct
 * @constructor
 * @template T
 */
r5js.ast.SimpleDatum = function(payload) {
  goog.base(this);

  /** @private {T} */ this.payload_ = payload;
};
goog.inherits(r5js.ast.SimpleDatum, r5js.ast.Literal);


/**
 * Booleans, characters, and numbers have value equality semantics.
 * @override
 */
r5js.ast.SimpleDatum.prototype.eqv = function(other) {
  return this.constructor === other.constructor && this.payload_ ===
      (/** @type {!r5js.ast.SimpleDatum} */ (other)).payload_;
};


/** @return {T} */
r5js.ast.SimpleDatum.prototype.getPayload = function() {
  return this.payload_;
};


/** @param {T} payload */
r5js.ast.SimpleDatum.prototype.setPayload = function(payload) {
  this.payload_ = payload;
};


/** @override */
r5js.ast.SimpleDatum.prototype.clone = function(parent) {
  var clone = /** @type {!r5js.ast.SimpleDatum} */ (
      goog.base(this, 'clone', parent));
  clone.setPayload(this.payload_);
  return clone;
};


/** @override */
r5js.ast.SimpleDatum.prototype.isEqual = function(other) {
  return other instanceof r5js.ast.SimpleDatum &&
      this.payload_ === other.payload_;
};


/** @override */
r5js.ast.SimpleDatum.prototype.unwrap = function() {
  return this.payload_;
};
