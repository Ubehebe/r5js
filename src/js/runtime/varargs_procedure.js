goog.provide('r5js.VarargsProcedure');


goog.require('r5js.Procedure');
goog.require('r5js.ProcedureLike');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.TooFewArgs');
goog.require('r5js.ast.List');
goog.require('r5js.datumutil');



/**
 * @param {!Array.<string>} formalsArray The procedure's formal parameters,
 * in order.
 * @param {r5js.Datum} bodyStart
 * @param {!r5js.IEnvironment} env
 * @param {string=} opt_name The procedure's name, for pretty-printing and
 * error messages. If not given, one will be created.
 * @implements {r5js.ProcedureLike}
 * @extends {r5js.Procedure}
 * @struct
 * @constructor
 */
r5js.VarargsProcedure = function(formalsArray, bodyStart, env, opt_name) {
  goog.base(this, formalsArray, bodyStart, env, opt_name);
};
goog.inherits(r5js.VarargsProcedure, r5js.Procedure);
r5js.ProcedureLike.addImplementation(r5js.VarargsProcedure);


/** @override */
r5js.VarargsProcedure.prototype.checkNumArgs = function(numActuals) {
  var minNumArgs = this.formalsArray.length - 1;
  if (numActuals < minNumArgs) {
    throw new r5js.TooFewArgs(this.toString(), minNumArgs, numActuals);
  }
};


/** @override */
r5js.VarargsProcedure.prototype.bindArgs = function(args, env) {
  var name, i;

  for (i = 0; i < this.formalsArray.length - 1; ++i) {
    name = this.formalsArray[i];
    env.addBinding(name, args[i]);
  }

  if (this.formalsArray.length > 0) {
    name = this.formalsArray[i];
    // Roll up the remaining arguments into a list
    var siblingBuffer = new r5js.SiblingBuffer();
    for (var j = this.formalsArray.length - 1; j < args.length; ++j) {
      siblingBuffer.appendSibling(r5js.datumutil.maybeWrapResult(args[j]));
    }
    env.addBinding(name, siblingBuffer.toList(r5js.ast.List));
  }
};
