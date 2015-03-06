/* Copyright 2011-2015 Brendan Linn

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


goog.provide('r5js.curPlatform');


goog.require('r5js.Platform');
goog.require('r5js.platform.common.newEvaluator');



/**
 * @implements {r5js.Platform}
 * @struct
 * @constructor
 * @private
 */
r5js.platform.Nashorn_ = function() {};


/** @override */
r5js.platform.Nashorn_.prototype.exit = goog.nullFunction; // TODO bl?


/** @override */
r5js.platform.Nashorn_.prototype.newEvaluator =
    r5js.platform.common.newEvaluator;


/** @return {!r5js.Platform} */
r5js.curPlatform = function() {
  return new r5js.platform.Nashorn_();
};
