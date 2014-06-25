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

goog.provide('haveJsOutput');
goog.provide('r5js.test.matchers.HasJsOutput_');
goog.setTestOnly('haveJsOutput');


/**
 * @param {?} output
 * @return {!tdd.matchers.Matcher}
 */
haveJsOutput = function(output) {
  return new r5js.test.matchers.HasJsOutput_(
      output, /** @type {!r5js.OutputSavingPort} */ (
      r5js.test.matchers.HasJsOutput_.sharedOutputPort_));
};



/**
 * @param {?} expectedOutput
 * @param {!r5js.OutputSavingPort} outputPort
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.HasJsOutput_ = function(expectedOutput, outputPort) {
  /** @const @private */ this.expectedOutput_ = expectedOutput;
  /** @private {?} */ this.actualOutput_ = null;
  /** @const @private */ this.outputPort_ = outputPort;
};


/** @private {r5js.OutputSavingPort} */
r5js.test.matchers.HasJsOutput_.sharedOutputPort_ = null;


/** @override */
r5js.test.matchers.HasJsOutput_.prototype.matches = function(input) {
  this.actualOutput_ = this.outputPort_.dequeueOutput();
  return r5js.test.matchers.HasJsValue_.equals(
      r5js.test.matchers.HasJsValue_.prepare(this.actualOutput_),
      this.expectedOutput_);
};


/** @override */
r5js.test.matchers.HasJsOutput_.prototype.getFailureMessage = function(input) {
  return 'want ' + this.expectedOutput_ + ' got ' + this.actualOutput_.value;
};
