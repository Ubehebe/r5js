goog.provide('r5js.ast.Lambda');


goog.require('r5js.ast.SimpleDatum');
goog.require('r5js.parse.Terminals');



/**
 * @param {string} name Name of the procedure.
 * @param {!r5js.Procedure} procedure TODO bl.
 * @extends {r5js.ast.SimpleDatum.<!r5js.Procedure>}
 * @struct
 * @constructor
 */
r5js.ast.Lambda = function(name, procedure) {
  goog.base(this, procedure);

  /** @const @private */ this.name_ = name;
};
goog.inherits(r5js.ast.Lambda, r5js.ast.SimpleDatum);


/** @return {string} */
r5js.ast.Lambda.prototype.getName = function() {
  return this.name_;
};
