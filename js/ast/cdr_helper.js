goog.module('r5js.ast.CdrHelper');

const {Datum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');

/**
 * See the comment to {@link r5js.Datum.siblingsToList}
 * for an explanation of what this interface does.
 * @interface
 */
class CdrHelper {

    /**
     * Basically, call set-car! on the master list.
     * @param {!Datum} car
     */
    setCar(car) {}

    /**
     * Basically, call set-cdr! on the master list.
     * @param {!Datum} cdr
     */
    setCdr(cdr) {}

    /**
     * @param {!CdrHelper} cdrHelper Helper to compare against.
     * @return {boolean} True iff the two CdrHelpers point to the same list
     * and have the same offset.
     */
    equals(cdrHelper) {}

    /**
     * @param {!Datum} datum The datum to test against.
     * @return {boolean} True iff the CdrHelper points to the given list datum
     * and its offset is that list's first child.
     */
    resolvesTo(datum) {}

}

exports = CdrHelper;