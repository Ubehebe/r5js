goog.module('r5js.ast.Character');

const SimpleDatum = goog.require('r5js.ast.SimpleDatum');

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
