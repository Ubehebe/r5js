goog.provide('r5js.VarargsProcedure');


goog.require('r5js.Procedure');
goog.require('r5js.ProcedureLike');



/**
 * @param {!Array.<string>} formalsArray The procedure's formal parameters,
 * in order.
 * @param {?} bodyStart TODO bl.
 * @param {!r5js.IEnvironment} env
 * @param {string=} opt_name The procedure's name, for pretty-printing and
 * error messages. If not given, one will be created.
 * @implements {r5js.ProcedureLike}
 * @extends {r5js.Procedure}
 * @struct
 * @constructor
 */
r5js.VarargsProcedure = function(formalsArray, bodyStart, env, opt_name) {
  goog.base(this, formalsArray, true, bodyStart, env, opt_name);
};
goog.inherits(r5js.VarargsProcedure, r5js.Procedure);
r5js.ProcedureLike.addImplementation(r5js.VarargsProcedure);
