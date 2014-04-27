goog.provide('r5js.ast.Lambda');


goog.require('r5js.ast.SimpleDatum');
goog.require('r5js.parse.Terminals');



/**
 * @param {string} name Name of the procedure.
 * @param {!r5js.AbstractProcedure} procedure TODO bl.
 * @extends {r5js.ast.SimpleDatum.<!r5js.AbstractProcedure>}
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


/** @override */
r5js.ast.Lambda.prototype.stringForOutputMode = function(outputMode) {
  return this.name_;
};
