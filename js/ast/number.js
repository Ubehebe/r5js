goog.module('r5js.ast.Number');

const {SimpleDatum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/simple_datum');

/** @extends {SimpleDatum<number>} */
class Number extends SimpleDatum {
 /** @param {number} x */
  constructor(x) {
   super(x);
  }
}

exports = Number;
