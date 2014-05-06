goog.provide('r5js.ast.String');


goog.require('r5js.DatumType');
goog.require('r5js.OutputMode');
goog.require('r5js.ast.SimpleDatum');



/**
 * @param {string} s
 * @extends {r5js.ast.SimpleDatum.<string>}
 * @struct
 * @constructor
 */
r5js.ast.String = function(s) {
  goog.base(this, s);
};
goog.inherits(r5js.ast.String, r5js.ast.SimpleDatum);


/**
 * Unlike other simple datums, strings have reference equality semantics.
 * @see R5RS 6.1
 * @override
 */
r5js.ast.String.prototype.eqv = function(other) {
  return this === other;
};


/**
 * Datums representing strings have payloads of type string.
 * If they all unwrapped as JavaScript strings, it would be impossible
 * to re-wrap them correctly (noninjective mapping). We choose to store
 * identifiers unwrapped because they're expected to be more common than
 * strings.
 *
 * @override
 */
r5js.ast.String.prototype.unwrap = function() {
  return this;
};
