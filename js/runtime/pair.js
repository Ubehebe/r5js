goog.module('r5js.runtime.Pair');

const NIL = goog.require('r5js.runtime.NIL');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');

/** @implements {ObjectValue} */
class Pair {
    /**
     * @param {!Value} car
     * @param {!Value=} cdr
     */
    constructor(car, cdr=NIL) {
        this.car_ = car;
        this.cdr_ = cdr;
    }

    /**
     * @param {!Value} other
     * @return {boolean}
     */
    eqv(other) {
        return this === other;
    }

    /**
     * @param {function(!Value): !Value} f
     * @return {!Pair}
     */
    map(f) {
        if (this.cdr_ === NIL) {
            return new Pair(f(this.car_));
        } else if (this.cdr_ instanceof Pair) {
            return new Pair(
                f(this.car_),
                (/** @type {!Pair} */ (this.cdr_)).map(f));
        } else {
            throw Error.internalInterpreterError('not a list!');
        }
    }

    /** @return {!Value} */
    car() {
        return this.car_;
    }

    /** @return {!Value} */
    cdr() {
        return this.cdr_;
    }

    /** @param {!Value} car */
    setCar(car) {
        this.car_ = car;
    }

    /** @param {!Value} cdr */
    setCdr(cdr) {
        this.cdr_ = cdr;
    }

    /**
     * @param {!Array<!Value>} array
     * @return {!Value}
     */
    fromArray(array) {
        if (!array.length) {
            return NIL;
        }
        const cars = array.map(elem => new Pair(elem));
        for (let i = 0; i < cars.length - 1; ++i) {
            cars[i].setCdr(cars[i + 1]);
        }
        return cars[0];
    }

    /** @return {boolean} */
    isList() {
        return this.cdr_ === NIL
            || (this.cdr_ instanceof Pair
            && (/** @type {!Pair} */(this.cdr_)).isList());
    }
}

exports = Pair;
