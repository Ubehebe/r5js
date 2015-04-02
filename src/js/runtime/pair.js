goog.provide('r5js.runtime.Pair');


goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.List');
goog.require('r5js.error');
goog.require('r5js.runtime.NIL');



r5js.runtime.Pair = /** @implements {r5js.runtime.ObjectValue} */ class {
    /**
     * @param {!r5js.runtime.Value} car
     * @param {!r5js.runtime.Value=} opt_cdr
     */
    constructor(car, opt_cdr) {
        this.car_ = car;
        this.cdr_ = goog.isDef(opt_cdr) ? opt_cdr : r5js.runtime.NIL;
    }

    /**
     * @param {!r5js.runtime.Value} other
     * @return {boolean}
     */
    eqv(other) {
        return this === other;
    }

    /**
     * @param {function(!r5js.runtime.Value): !r5js.runtime.Value} f
     * @return {!r5js.runtime.Pair}
     */
    map(f) {
        if (this.cdr_ === r5js.runtime.NIL) {
            return new r5js.runtime.Pair(f(this.car_));
        } else if (this.cdr_ instanceof r5js.runtime.Pair) {
            return new r5js.runtime.Pair(
                f(this.car_),
                (/** @type {!r5js.runtime.Pair} */ (this.cdr_)).map(f));
        } else {
            throw new r5js.Error(
                r5js.Error.Type.INTERNAL_INTERPRETER_ERROR, 'not a list!');
        }
    }

    /** @return {!r5js.runtime.Value} */
    car() {
        return this.car_;
    }

    /** @return {!r5js.runtime.Value} */
    cdr() {
        return this.cdr_;
    }

    /** @param {!r5js.runtime.Value} car */
    setCar(car) {
        this.car_ = car;
    }

    /** @param {!r5js.runtime.Value} cdr */
    setCdr(cdr) {
        this.cdr_ = cdr;
    }

    /**
     * @param {!Array<!r5js.runtime.Value>} array
     * @return {!r5js.runtime.Value}
     */
    fromArray(array) {
        if (!array.length) {
            return r5js.runtime.NIL;
        }
        var cars = array.map(function (elem) {
            return new r5js.runtime.Pair(elem);
        });
        for (var i = 0; i < cars.length - 1; ++i) {
            cars[i].setCdr(cars[i + 1]);
        }
        return cars[0];
    }

    /** @return {boolean} */
    isList() {
        return this.cdr_ === r5js.runtime.NIL ||
            (this.cdr_ instanceof r5js.runtime.Pair &&
            (/** @type {!r5js.runtime.Pair} */(this.cdr_)).isList());
    }
};
