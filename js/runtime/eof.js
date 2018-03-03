goog.module('r5js.runtime.EOF');

const {Datum} = require('/js/macro/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');

/** @type {!Value} */ const EOF = new Datum();

exports = EOF;