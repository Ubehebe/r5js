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

goog.provide('r5js.R5RSCompliantOutputPort');


goog.require('r5js.OutputPort');



/**
 * {@link r5js.OutputPort} implementation that uses the official R5RS
 * serializations for writing and displaying Scheme values.
 * @param {function(string)} onOutput Function that will be called whenever
 * output is available.
 * @implements {r5js.OutputPort}
 * @struct
 * @constructor
 */
r5js.R5RSCompliantOutputPort = function(onOutput) {
  /** @const @private */ this.onOutput_ = onOutput;
};
r5js.OutputPort.addImplementation(r5js.R5RSCompliantOutputPort);


/** @override */
r5js.R5RSCompliantOutputPort.prototype.close = goog.nullFunction;


/** @override */
r5js.R5RSCompliantOutputPort.prototype.write = function(str) {
  this.onOutput_(str);
};
