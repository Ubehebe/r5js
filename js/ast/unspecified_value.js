goog.module('r5js.UNSPECIFIED_VALUE');

const {Datum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');

/** @type {!Value} */ const UNSPECIFIED_VALUE = new Datum();

exports = UNSPECIFIED_VALUE;