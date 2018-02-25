goog.module('r5js.parse.bnf.Rule');

const {Datum} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const DatumStream = goog.require('r5js.DatumStream');

/** @interface */
class Rule {
    /**
     * @param {!DatumStream} datumStream
     * @return {boolean|!Datum} True iff the parse succeeded.
     */
    match(datumStream) {}
}

exports = Rule;