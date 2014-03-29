goog.provide('r5js.ast.Lambda');


goog.require('r5js.Datum');
goog.require('r5js.PrimitiveProcedure');
goog.require('r5js.parse.Terminals');



/**
 * @param {string} name Name of the procedure.
 * @param {!r5js.PrimitiveProcedure|!r5js.Procedure} procedure TODO bl.
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.ast.Lambda = function(name, procedure) {
  goog.base(this);
  this.setPayload(procedure);

  /** @const @private */ this.name_ = name;
};
goog.inherits(r5js.ast.Lambda, r5js.Datum);


/** @return {string} */
r5js.ast.Lambda.prototype.getName = function() {
  return this.name_;
};


/** @override */
r5js.ast.Lambda.prototype.stringForOutputMode = function(outputMode) {
  return r5js.PrimitiveProcedure.isImplementedBy(
      this.getPayload()) ? this.name_ :
      'proc:' + this.getPayload().name;
};
