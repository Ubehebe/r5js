goog.provide('r5js.Ref');


goog.require('r5js.ast.SimpleDatum');



/**
 * TODO bl this class should not exist. It's used only as a shim in
 * {@link r5js.Environment#get}.
 * @param {!r5js.Datum} deref Datum to dereference.
 * @extends {r5js.ast.SimpleDatum.<!r5js.Datum>}
 * @struct
 * @constructor
 */
r5js.Ref = function(deref) {
  goog.base(this, deref);
};
goog.inherits(r5js.Ref, r5js.ast.SimpleDatum);


/** @return {!r5js.Datum} */
r5js.Ref.prototype.deref = function() {
  return /** @type {!r5js.Datum} */ (this.getPayload());
};


/** @override */
r5js.Ref.prototype.stringForOutputMode = function(outputMode) {
  return this.getPayload().stringForOutputMode(outputMode);
};
