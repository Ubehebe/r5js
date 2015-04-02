goog.provide('r5js.runtime.NIL');

r5js.runtime.Nil_ = /** @private @implements {r5js.runtime.ObjectValue} */ class {
 /**
  * @param {!r5js.runtime.Value} other
  * @return {boolean}
  */
 eqv(other) {
  return this === other;
 }
};

/** @type {!r5js.runtime.Value} */
r5js.runtime.NIL = new r5js.runtime.Nil_();
