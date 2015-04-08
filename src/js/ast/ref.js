goog.module('r5js.Ref');

const Datum = goog.require('r5js.Datum');
const SimpleDatum = goog.require('r5js.ast.SimpleDatum');

/**
 * TODO bl this class should not exist. It's used only as a shim in
 * {@link r5js.Environment#get}.
 * @extends {SimpleDatum<!Datum>}
 */
class Ref extends SimpleDatum {
    /** @param {!Datum} deref Datum to dereference. */
    constructor(deref) {
        super(deref);
    }

    /** @return {!Datum} */
    deref() {
        return this.payload;
    }
}

exports = Ref;
