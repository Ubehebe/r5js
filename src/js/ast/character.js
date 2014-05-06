goog.provide('r5js.ast.Character');


goog.require('r5js.DatumType');
goog.require('r5js.ast.SimpleDatum');



/**
 * @param {string} c
 * @extends {r5js.ast.SimpleDatum.<string>}
 * @struct
 * @constructor
 */
r5js.ast.Character = function(c) {
  goog.base(this, c);
};
goog.inherits(r5js.ast.Character, r5js.ast.SimpleDatum);


/**
 * Datums representing characters have payloads of type string.
 * If they unwrapped as JavaScript strings, it would be impossible
 * to re-wrap them correctly (noninjective mapping). We choose to store
 * identifiers unwrapped because they're expected to be more common than
 * the other two.
 *
 * @override
 */
r5js.ast.Character.prototype.unwrap = function() {
  return this;
};
