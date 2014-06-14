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

goog.provide('r5js.OutputSavingPort');


goog.require('r5js.OutputPort');



/**
 * @implements {r5js.OutputPort}
 * @struct
 * @constructor
 * @template T
 */
r5js.OutputSavingPort = function() {
  /** @private {!Array.<!r5js.JsonValue>} */ this.values_ = [];
};
r5js.OutputPort.addImplementation(r5js.OutputSavingPort);


/** @override */
r5js.OutputSavingPort.prototype.write = function(value) {
  this.values_.push(value);
};


/** @override */
r5js.OutputSavingPort.prototype.close = goog.nullFunction;


/** @return {!Array.<T>} */
r5js.OutputSavingPort.prototype.getAndClearOutput = function() {
  var values = this.values_;
  this.values_ = [];
  return values;
};


