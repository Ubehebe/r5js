goog.module('r5js.IPair');

const Value = goog.require('r5js.runtime.Value');

/** @interface */
class IPair {
    /** @return {!Value} */ car() {}
    /** @return {!Value} */ cdr() {}

    /**
     * @param {*} obj
     * @return {boolean}
     */
    static isImplementedBy(obj) {
        return !!(obj && obj[IMPLEMENTED_BY_PROP]);
    }

    /** @param {function(new: IPair, ...)} ctor */
    static addImplementation(ctor) {
        ctor.prototype[IMPLEMENTED_BY_PROP] = true;
    }
}

/** @const */ const IMPLEMENTED_BY_PROP = '$r5js.IPair';

exports = IPair;