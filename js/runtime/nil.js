goog.module('r5js.runtime.NIL');

const ObjectValue = goog.require('r5js.runtime.ObjectValue');
const Value = goog.require('r5js.runtime.Value');

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

/** @type {!Value} */ const NIL = new Nil();

exports = NIL;