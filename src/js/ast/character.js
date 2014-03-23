goog.provide('r5js.ast.Character');


goog.require('r5js.DatumType');
goog.require('r5js.OutputMode');
goog.require('r5js.ast.SimpleDatum');



/**
 * @param {string} c
 * @extends {r5js.ast.SimpleDatum}
 * @struct
 * @constructor
 */
r5js.ast.Character = function(c) {
  goog.base(this);
  this.setPayload(c);
  this.setType(r5js.DatumType.CHARACTER); // TODO bl remove
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


/** @override */
r5js.ast.Character.prototype.stringForOutputMode = function(outputMode) {
  switch (outputMode) {
    case r5js.OutputMode.WRITE:
      if (this.getPayload() === ' ')
        return '#\\space';
      else if (this.getPayload() === '\n')
        return '#\\newline';
      else
        return '#\\' + this.getPayload();
    case r5js.OutputMode.DISPLAY:
    default:
      return /** @type {string} */(this.getPayload());
  }
};
