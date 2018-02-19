goog.module('r5js.ast.Boolean');

const {SimpleDatum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');

/** @extends {SimpleDatum<boolean>} */
class Boolean extends SimpleDatum {
 /** @param {boolean} val */
 constructor(val) {
  super(val);
 }
}

exports = Boolean;