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

goog.provide('haveStringOutput');
goog.provide('r5js.test.matchers.setOutputPort');
goog.setTestOnly('haveStringOutput');
goog.setTestOnly('r5js.test.matchers.setOutputPort');


goog.require('goog.array');
goog.require('r5js.OutputSavingPort');
goog.require('r5js.test.matchers.HasJsValue_');
goog.require('r5js.test.matchers.Throws');
goog.require('r5js.valutil');


/**
 * @param {string} output
 * @return {!tdd.matchers.Matcher}
 */
haveStringOutput = function(output) {
  return new r5js.test.matchers.HasStringOutput_(
      output,
      /** @type {!r5js.OutputSavingPort} */ (
          r5js.test.matchers.HasStringOutput_.sharedOutputPort_));
};



/**
 * @param {string} expectedOutput
 * @param {!r5js.OutputSavingPort} outputPort
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.HasStringOutput_ = function(expectedOutput, outputPort) {
  /** @const @private */ this.expectedOutput_ = expectedOutput;
  /** @private {?string} */ this.actualOutput_ = null;
  /** @const @private */ this.outputPort_ = outputPort;
};


/** @private {r5js.OutputSavingPort} */
r5js.test.matchers.HasStringOutput_.sharedOutputPort_ = null;


/** @override */
r5js.test.matchers.HasStringOutput_.prototype.matches = function(input) {
  this.actualOutput_ = this.outputPort_.getAndClearOutput()[0].displayValue;
  return this.actualOutput_ === this.expectedOutput_;
};


/** @override */
r5js.test.matchers.HasStringOutput_.prototype.getFailureMessage =
    function(input) {
  return 'want ' + this.expectedOutput_ + ' got ' + this.actualOutput_;
};


/**
 * @param {!r5js.OutputSavingPort} outputPort
 * @suppress {accessControls}
 */
r5js.test.matchers.setOutputPort = function(outputPort) {
  r5js.test.matchers.HasJsOutput_.sharedOutputPort_ =
      r5js.test.matchers.HasStringOutput_.sharedOutputPort_ = outputPort;
};
