/* Copyright 2011-2014 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

goog.provide('r5js.VarargsUserDefinedProcedure');


goog.require('r5js.SiblingBuffer');
goog.require('r5js.UserDefinedProcedure');
goog.require('r5js.ast.List');
goog.require('r5js.datumutil');
goog.require('r5js.error');



/**
 * @param {!Array.<string>} formalsArray The procedure's formal parameters,
 * in order.
 * @param {r5js.Datum} bodyStart
 * @param {!r5js.IEnvironment} env
 * @param {string=} opt_name The procedure's name, for pretty-printing and
 * error messages. If not given, one will be created.
 * @extends {r5js.UserDefinedProcedure}
 * @struct
 * @constructor
 */
r5js.VarargsUserDefinedProcedure = function(
    formalsArray, bodyStart, env, opt_name) {
  goog.base(this, formalsArray, bodyStart, env, opt_name);
};
goog.inherits(r5js.VarargsUserDefinedProcedure, r5js.UserDefinedProcedure);


/** @override */
r5js.VarargsUserDefinedProcedure.prototype.checkNumArgs = function(numActuals) {
  var minNumArgs = this.formalsArray.length - 1;
  if (numActuals < minNumArgs) {
    throw r5js.error.tooFewVarargs(this.toString(), minNumArgs, numActuals);
  }
};


/** @override */
r5js.VarargsUserDefinedProcedure.prototype.bindArgs = function(args, env) {
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
      siblingBuffer.appendSibling(r5js.datumutil.wrapValue(args[j]));
    }
    env.addBinding(name, siblingBuffer.toList(r5js.ast.List));
  }
};
