goog.module('r5js.IPair');

/** @interface */
class IPair {
    /** @return {!Value} */ car() {}
    /** @return {!Value} */ cdr() {}
}

const IMPLEMENTED_BY_PROP = '$r5js.IPair';

/**
 * @param {*} obj
 * @return {boolean}
 */
function isImplementedBy(obj) {
    return !!(obj && obj[IMPLEMENTED_BY_PROP]);
}

/** @param {function(new: IPair, ...)} ctor */
function addImplementation(ctor) {
    ctor.prototype[IMPLEMENTED_BY_PROP] = true;
}

exports = {IPair, isImplementedBy, addImplementation};