goog.module('r5js.runtime.NIL');

const {ObjectValue, Value} = goog.require('r5js.Value');

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