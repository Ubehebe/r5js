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

goog.provide('haveStringOutput');
goog.provide('r5js.test.matchers.setSharedEvaluator');
goog.setTestOnly('haveJsOutput');
goog.setTestOnly('r5js.test.matchers.setSharedEvaluator');


goog.require('goog.array');
goog.require('r5js.OutputSavingPort');
goog.require('r5js.datumutil');
goog.require('r5js.test.matchers.Throws');
goog.require('r5js.valutil');


/**
 * @param {?} output
 * @return {!tdd.matchers.Matcher}
 */
haveJsOutput = function(output) {
  return new r5js.test.matchers.HasJsOutput_(output,
      /** @type {!r5js.Evaluator} */(
      r5js.test.matchers.HasJsOutput_.sharedEvaluator_),
      r5js.test.matchers.HasJsOutput_.sharedOutputPort_);
};


/**
 * @param {string} output
 * @return {!tdd.matchers.Matcher}
 */
haveStringOutput = function(output) {
  return new r5js.test.matchers.HasStringOutput_(output,
      /** @type {!r5js.Evaluator} */ (
      r5js.test.matchers.HasStringOutput_.sharedEvaluator_),
      r5js.test.matchers.HasStringOutput_.sharedOutputPort_);
};



/**
 * @param {?} expectedOutput
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.OutputSavingPort} outputPort
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.HasJsOutput_ = function(
    expectedOutput, evaluator, outputPort) {
  /** @const @private */ this.expectedOutput_ = expectedOutput;
  /** @const @private */ this.evaluator_ = evaluator;
  /** @const @private */ this.outputPort_ = outputPort;
};


/** @private {r5js.sync.Evaluator} */
r5js.test.matchers.HasJsOutput_.sharedEvaluator_;


/**
 * @private {!r5js.OutputSavingPort}
 * @const
 */
r5js.test.matchers.HasJsOutput_.sharedOutputPort_ = new r5js.OutputSavingPort();


/** @override */
r5js.test.matchers.HasJsOutput_.prototype.matches = function(input) {
  this.evaluator_.evaluate(/** @type {string} */ (input));
  var actualOutput = this.outputPort_.getAndClearOutput()[0];
  return r5js.test.matchers.HasJsValue_.equals(
      actualOutput, this.expectedOutput_);
};


/** @override */
r5js.test.matchers.HasJsOutput_.prototype.getSuccessMessage = function(input) {
  return 'ok';
};


/** @override */
r5js.test.matchers.HasJsOutput_.prototype.getFailureMessage = function(input) {
  return 'want ' +
      this.expectedOutput_ +
      ' got ';
};



/**
 * @param {string} expectedOutput
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.OutputSavingPort} outputPort
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.HasStringOutput_ = function(
    expectedOutput, evaluator, outputPort) {
  /** @const @private */ this.expectedOutput_ = expectedOutput;
  /** @const @private */ this.evaluator_ = evaluator;
  /** @const @private */ this.outputPort_ = outputPort;
};


/** @private {r5js.sync.Evaluator} */
r5js.test.matchers.HasStringOutput_.sharedEvaluator_;


/** @const @private {!r5js.OutputSavingPort} */
r5js.test.matchers.HasStringOutput_.sharedOutputPort_ =
    new r5js.OutputSavingPort();


/** @override */
r5js.test.matchers.HasStringOutput_.prototype.matches = function(input) {
  this.evaluator_.evaluate(/** @type {string} */ (input));
  var actualOutput = this.outputPort_.getAndClearOutput()[0];
  return actualOutput === this.expectedOutput_;
};


/** @override */
r5js.test.matchers.HasStringOutput_.prototype.getSuccessMessage =
    function(input) {
  return 'ok';
};


/** @override */
r5js.test.matchers.HasStringOutput_.prototype.getFailureMessage =
    function(input) {
  return 'want ' +
      this.expectedOutput_ +
      ' got ';
};


/** @param {!r5js.sync.Evaluator} evaluator */
r5js.test.matchers.setSharedEvaluator = function(evaluator) {
  r5js.test.matchers.HasJsOutput_.sharedEvaluator_ = evaluator.withPorts(
      r5js.InputPort.NULL, r5js.test.matchers.HasJsOutput_.sharedOutputPort_);
  r5js.test.matchers.HasStringOutput_.sharedEvaluator_ = evaluator.withPorts(
      r5js.InputPort.NULL,
      r5js.test.matchers.HasStringOutput_.sharedOutputPort_);
  r5js.test.matchers.Throws.sharedEvaluator = evaluator;
};
