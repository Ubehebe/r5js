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

goog.provide('Throw');
goog.provide('Throw2');
goog.provide('r5js.test.matchers.Throws');
goog.provide('r5js.test.matchers.Throws2');
goog.setTestOnly('Throw');
goog.setTestOnly('Throw2');
goog.setTestOnly('r5js.test.matchers.Throws');
goog.setTestOnly('r5js.test.matchers.Throws2');


/**
 * @param {function(new: r5js.Error, ...)} exceptionCtor
 * @return {!tdd.matchers.Matcher}
 */
Throw = function(exceptionCtor) {
  return new r5js.test.matchers.Throws(
      exceptionCtor, /** @type {!r5js.Evaluator} */ (
      r5js.test.matchers.Throws.sharedEvaluator));
};


/**
 * @param {!r5js.Error} error
 * @return {!tdd.matchers.Matcher}
 */
Throw2 = function(error) {
  return new r5js.test.matchers.Throws2(
      error, /** @type {!r5js.Evaluator} */ (
          r5js.test.matchers.Throws2.sharedEvaluator));
};



/**
 * @param {function(new: r5js.Error)} exceptionCtor
 * @param {!r5js.Evaluator} evaluator
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 */
r5js.test.matchers.Throws = function(exceptionCtor, evaluator) {
  /** @const @private */ this.exceptionCtor_ = exceptionCtor;
  /** @private {r5js.Error} */ this.actualException_ = null;
  /** @const @private */ this.evaluator_ = evaluator;
};


/** @type {r5js.Evaluator} */
r5js.test.matchers.Throws.sharedEvaluator = null;


/** @override */
r5js.test.matchers.Throws.prototype.matches = function(input) {
  try {
    this.evaluator_.evaluate(/** @type {string} */(input));
  } catch (e) {
    return (this.actualException_ = e) instanceof
        this.exceptionCtor_;
  }
  return false;
};


/** @override */
r5js.test.matchers.Throws.prototype.getSuccessMessage = function(input) {
  return 'ok';
};


/** @override */
r5js.test.matchers.Throws.prototype.getFailureMessage = function(input) {
  return input + ': want ' +
      new this.exceptionCtor_().getShortName() +
      ' got ' +
      (this.actualException_ ?
      this.actualException_.getShortName() :
      'no exception');
};



/**
 * @param {!r5js.Error} expectedError
 * @param {!r5js.Evaluator} evaluator
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 */
r5js.test.matchers.Throws2 = function(expectedError, evaluator) {
  /** @const @private */ this.expectedError_ = expectedError;
  /** @private */ this.actualError_ = null;
  /** @const @private */ this.evaluator_ = evaluator;
};


/** @type {r5js.Evaluator} */
r5js.test.matchers.Throws2.sharedEvaluator = null;


/** @override */
r5js.test.matchers.Throws2.prototype.matches = function(input) {
  try {
    this.evaluator_.evaluate(/** @type {string} */ (input));
  } catch (e) {
    return this.expectedError_.equals(this.actualError_ = e);
  }
  return false;
};


/** @override */
r5js.test.matchers.Throws2.prototype.getSuccessMessage = function(input) {
  return 'ok';
};


/** @override */
r5js.test.matchers.Throws2.prototype.getFailureMessage = function(input) {
  return input +
      ': want\n' +
      this.expectedError_.toString() +
      '\ngot ' +
      (this.actualError_ ?
          ('\n' + this.actualError_.toString()) :
          'no exception');
};