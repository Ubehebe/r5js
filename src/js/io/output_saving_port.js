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
 * @param {function(!r5js.runtime.Value):T} writeConverter Function used to
 * convert values passed to (write x).
 * @param {function(!r5js.runtime.Value):T=} opt_displayConverter Function used
 * to convert values passed to (display x). If not given, writeConverter
 * will be used.
 * @implements {r5js.OutputPort}
 * @struct
 * @constructor
 * @template T
 */
r5js.OutputSavingPort = function(writeConverter, opt_displayConverter) {
  /** @const @private */ this.writeConverter_ = writeConverter;
  /** @const @private */ this.displayConverter_ =
      opt_displayConverter || writeConverter;
  /** @private {!Array.<T>} */ this.values_ = [];
};
r5js.OutputPort.addImplementation(r5js.OutputSavingPort);


/** @override */
r5js.OutputSavingPort.prototype.writeValue = function(value) {
  this.values_.push(this.writeConverter_(value));
};


/** @override */
r5js.OutputSavingPort.prototype.writeChar = goog.nullFunction;


/** @override */
r5js.OutputSavingPort.prototype.display = function(value) {
  this.values_.push(this.displayConverter_(value));
};


/** @override */
r5js.OutputSavingPort.prototype.close = goog.nullFunction;


/** @return {!Array.<T>} */
r5js.OutputSavingPort.prototype.getAndClearOutput = function() {
  var values = this.values_;
  this.values_ = [];
  return values;
};


