goog.module('r5js.ast.Literal');

const {Datum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');

class Literal extends Datum {
  constructor() {
      super();
  }

  // vacuous??
}

exports = Literal;