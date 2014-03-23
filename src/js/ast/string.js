goog.provide('r5js.ast.String');


goog.require('r5js.DatumType');
goog.require('r5js.OutputMode');
goog.require('r5js.ast.SimpleDatum');



/**
 * @param {string} s
 * @extends {r5js.ast.SimpleDatum}
 * @struct
 * @constructor
 */
r5js.ast.String = function(s) {
  goog.base(this);
  this.setType(r5js.DatumType.STRING); // TODO bl remove
  this.setPayload(s);
};
goog.inherits(r5js.ast.String, r5js.ast.SimpleDatum);


/** @override */
r5js.ast.String.prototype.stringForOutputMode = function(outputMode) {
  var payload = this.getPayload();
  return outputMode === r5js.OutputMode.DISPLAY ?
      payload :
      '"' + payload.replace(/([\\"])/g, '\\$1') + '"';
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
