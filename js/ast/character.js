goog.module('r5js.ast.Character');

const {SimpleDatum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/simple_datum');

/** @extends {SimpleDatum<string>} */
class Character extends SimpleDatum {

 /** @param {string} c */
 constructor(c) {
  super(c);
 }

 /**
  * Datums representing characters have payloads of type string.
  * If they unwrapped as JavaScript strings, it would be impossible
  * to re-wrap them correctly (noninjective mapping). We choose to store
  * identifiers unwrapped because they're expected to be more common than
  * the other two.
  *
  * @override
  */
 unwrap() {
  return this;
 }
}

exports = Character;
