goog.provide('r5js.ast.Identifier');


goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.ast.SimpleDatum');
goog.require('r5js.parse.Terminals');



/**
 * @param {string} name
 * @extends {r5js.ast.SimpleDatum}
 * @struct
 * @constructor
 */
r5js.ast.Identifier = function(name) {
  goog.base(this);
  this.setPayload(name);
  this.setType(r5js.DatumType.IDENTIFIER); // TODO bl remove
};
goog.inherits(r5js.ast.Identifier, r5js.ast.SimpleDatum);


/** @override */
r5js.ast.Identifier.prototype.stringForOutputMode = function(outputMode) {
  return /** @type {string} */ (this.getPayload());
};


/**
 * @return {boolean} True iff this Datum is in a quasiquotation and should be
 * unquoted (i.e. starts with a ,).
 */
r5js.ast.Identifier.prototype.shouldUnquote = function() {
  return this.getPayload().charAt(0) === r5js.parse.Terminals.COMMA;
};


/**
 * This is a subcase of shouldUnquote, because unquotes and unquote-splicings
 * have pretty much the same logic.
 * @return {boolean} TODO bl.
 * @suppress {accessControls} for r5js.Datum.CPS_PREFIX_
 */
r5js.ast.Identifier.prototype.shouldUnquoteSplice = function() {
  return this.getPayload().charAt(1) === r5js.Datum.CPS_PREFIX_;
};
