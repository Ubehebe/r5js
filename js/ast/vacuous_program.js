goog.module('r5js.VACUOUS_PROGRAM');

const {Datum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');

/**
 * According to the R5RS grammar, a sequence of zero datums is a valid program.
 * This object is used to prevent the interpreter from returning null
 * in contexts where that might erroneously be interpreted as an error.
 */
const VACUOUS_PROGRAM = new Datum();

exports = VACUOUS_PROGRAM;