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

  /** @protected {T} */ this.payload = payload;
};
goog.inherits(r5js.ast.SimpleDatum, r5js.ast.Literal);


/**
 * Booleans, characters, and numbers have value equality semantics.
 * @override
 */
r5js.ast.SimpleDatum.prototype.eqv = function(other) {
  return this.constructor === other.constructor && this.payload ===
      (/** @type {!r5js.ast.SimpleDatum} */ (other)).payload;
};


/** @return {T} */
r5js.ast.SimpleDatum.prototype.getPayload = function() {
  return this.payload;
};


/** @param {T} payload */
r5js.ast.SimpleDatum.prototype.setPayload = function(payload) {
  this.payload = payload;
};


/** @override */
r5js.ast.SimpleDatum.prototype.clone = function(parent) {
  var clone = /** @type {!r5js.ast.SimpleDatum} */ (
      goog.base(this, 'clone', parent));
  clone.setPayload(this.payload);
  return clone;
};


/**
 * TODO bl: this is intended to have the exact semantics of the library
 * procedure equal?, but I'm not sure that it does.
 * @param {!r5js.Datum} other Datum to compare against.
 * @return {boolean}
 */
r5js.ast.SimpleDatum.prototype.isEqual = function(other) {
  return other instanceof r5js.ast.SimpleDatum &&
      this.payload === other.payload;
};


/** @override */
r5js.ast.SimpleDatum.prototype.unwrap = function() {
  return this.payload;
};
