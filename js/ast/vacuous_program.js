goog.module('r5js.VACUOUS_PROGRAM');

const Datum = goog.require('r5js.Datum');

/**
 * According to the R5RS grammar, a sequence of zero datums is a valid program.
 * This object is used to prevent the interpreter from returning null
 * in contexts where that might erroneously be interpreted as an error.
 */
const VACUOUS_PROGRAM = new Datum();

exports = VACUOUS_PROGRAM;