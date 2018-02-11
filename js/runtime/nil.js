goog.module('r5js.runtime.NIL');

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