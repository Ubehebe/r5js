goog.module('r5js.parse.bnf.Rule');

const Datum = goog.require('r5js.Datum');
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