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

goog.provide('r5js.ast.Macro');


goog.require('r5js.ast.SimpleDatum');



/**
 * @param {!r5js.Macro} macro
 * @extends {r5js.ast.SimpleDatum<!r5js.Macro>}
 * @struct
 * @constructor
 */
r5js.ast.Macro = function(macro) {
  goog.base(this, macro);
};
goog.inherits(r5js.ast.Macro, r5js.ast.SimpleDatum);


/** @return {!r5js.Macro} */
r5js.ast.Macro.prototype.getMacro = function() {
  return this.payload.setIsLetOrLetrecSyntax();
};

