/* Copyright 2011, 2012 Brendan Linn

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


goog.provide('r5js.EnvBuffer');



/**
 * Just a pointer to an environment. It's separate from
 * {@link r5js.TrampolineHelper} to make it clear that old environments are only
 * reused in a few situations.
 * @struct
 * @constructor
 */
r5js.EnvBuffer = function() {
  /** @private {r5js.IEnvironment} */ this.env_ = null;
};


/** @return {r5js.IEnvironment} The environment, if any was set. */
r5js.EnvBuffer.prototype.getEnv = function() {
  return this.env_;
};


/** @param {!r5js.IEnvironment} env Environment to set. */
r5js.EnvBuffer.prototype.setEnv = function(env) {
  this.env_ = env;
};