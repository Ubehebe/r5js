goog.module('r5js.ast.SimpleDatum');

const {Datum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');

/** @template T */
class SimpleDatum extends Datum {
    /** @param {T} payload */
    constructor(payload) {
        super();
        /** @protected {T} */ this.payload = payload;
    }

    /**
     * Booleans, characters, and numbers have value equality semantics.
     * @override
     */
    eqv(other) {
        return this.constructor === other.constructor && this.payload ===
            (/** @type {!SimpleDatum} */ (other)).payload;
    }

    /** @return {T} */
    getPayload() {
        return this.payload;
    }

    /** @param {T} payload */
    setPayload(payload) {
        this.payload = payload;
    }

    /** @override */
    clone(parent) {
        var clone = /** @type {!SimpleDatum} */ (super.clone(parent));
        clone.setPayload(this.payload);
        return clone;
    }

    /**
     * TODO bl: this is intended to have the exact semantics of the library
     * procedure equal?, but I'm not sure that it does.
     * @param {!Datum} other Datum to compare against.
     * @return {boolean}
     */
    isEqual(other) {
        return other instanceof SimpleDatum
            && this.payload === other.payload;
    }

    /** @override */
    unwrap() {
        return this.payload;
    }
}

exports = SimpleDatum;
