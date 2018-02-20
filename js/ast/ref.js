goog.module('r5js.Ref');

const {Datum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {SimpleDatum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/simple_datum');

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

    /**
     * @return {!Datum}
     * @suppress {reportUnknownTypes}
     */
    deref() {
        return this.payload;
    }
}

exports = Ref;
