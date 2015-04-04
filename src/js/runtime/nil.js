goog.module('r5js.runtime.NIL');

const ObjectValue = goog.require('r5js.runtime.ObjectValue');

/** @implements {ObjectValue} */
class Nil {
 /**
  * @param {!Value} other
  * @return {boolean}
  */
 eqv(other) {
  return this === other;
 }
}

/** @type {!r5js.runtime.Value} */
const NIL = new Nil();

exports = NIL;