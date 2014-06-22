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

goog.provide('r5js.test.SyncPromiseTestSuite');
goog.setTestOnly('r5js.test.SyncPromiseTestSuite');


goog.require('goog.Promise');
goog.require('goog.functions');
goog.require('goog.string');
goog.require('tdd.LogLevel');
goog.require('tdd.LogRecord');
goog.require('tdd.ManualTestSuite');



/**
 *
 * @param {string} name
 * @implements {tdd.ManualTestSuite}
 * @constructor
 */
r5js.test.SyncPromiseTestSuite = function(name) {

  /** @const @private */ this.name_ = name;
  /** @private {goog.log.Logger} */ this.logger_ = null;
  /** @const @private {!Array.<string>} */
  this.testMethodNames_ = [];
  /** @const @private {!Array.<!Function>} */
  this.testMethods_ = [];
  /**
     * @private {!Object.<string,
     * !Array.<!r5js.test.SyncPromiseTestSuite.Expectation_>>}
     * @const
     */
  this.expectationsPerTestMethod_ = {};
  /** @private */ this.currentTestMethodName_ = '';
  /** @private */ this.curIndex_ = -1;
  /** @private */ this.curExpectationIndex_ = -1;
  /** @private */ this.numSucceeded_ = 0;
  /** @private */ this.numFailed_ = 0;
  /** @private */ this.numExceptions_ = 0;
};
tdd.ManualTestSuite.addImplementation(r5js.test.SyncPromiseTestSuite);


/** @override */
r5js.test.SyncPromiseTestSuite.prototype.getType = goog.functions.constant(
    tdd.TestType.UNIT);


/** @override */
r5js.test.SyncPromiseTestSuite.prototype.toString = function() {
  return this.name_;
};


/** @override */
r5js.test.SyncPromiseTestSuite.prototype.execute = function(logger) {
  this.logger_ = logger;
  for (var key in this) {
    var testMethod = this[key];
    if (r5js.test.SyncPromiseTestSuite.isTestMethod_(key, testMethod)) {
      this.testMethodNames_.push(key);
      this.testMethods_.push(testMethod);
      this.currentTestMethodName_ = key;
      this.expectationsPerTestMethod_[key] = [];
      // Call the test method synchronously to collect the promises.
      testMethod.call(this);
    }
  }
  return this.runNextTestMethod_();
};


/**
 * @return {!goog.Promise.<!tdd.ResultStruct>}
 * @private
 */
r5js.test.SyncPromiseTestSuite.prototype.runNextTestMethod_ = function() {
  if (++this.curIndex_ >= this.testMethods_.length) {
    return goog.Promise.resolve(
        new tdd.ResultStruct(
        this.numSucceeded_, this.numFailed_, this.numExceptions_));
  }

  this.currentTestMethodName_ = this.testMethodNames_[this.curIndex_];
  this.curExpectationIndex_ = -1;
  return this.runNextExpectation_();
};


/**
 * @return {!goog.Promise.<!tdd.ResultStruct>}
 * @private
 */
r5js.test.SyncPromiseTestSuite.prototype.runNextExpectation_ = function() {
  var expectation = this.getNextExpectation_();
  return expectation ?
      expectation.getPromise().
      then(this.onResolved_, this.onRejected_, this).
      then(this.runNextExpectation_, this.runNextExpectation_, this) :
      this.runNextTestMethod_();
};


/**
 * @return {r5js.test.SyncPromiseTestSuite.Expectation_} The next expectation
 * in the current test method, or null if there are no more.
 * @private
 */
r5js.test.SyncPromiseTestSuite.prototype.getNextExpectation_ = function() {
  var expectations =
      this.expectationsPerTestMethod_[this.currentTestMethodName_];
  return expectations[++this.curExpectationIndex_] || null;
};


/** @private */
r5js.test.SyncPromiseTestSuite.prototype.onResolved_ = function() {
  ++this.numSucceeded_;
  this.logger_.logRecord(new tdd.LogRecord(
      tdd.LogLevel.SUCCESS, this.name_,
      this.currentTestMethodName_));
};


/**
 * @param {*} rejectionReason
 * @private
 */
r5js.test.SyncPromiseTestSuite.prototype.onRejected_ = function(
    rejectionReason) {
  ++this.numFailed_;
  this.logger_.logRecord(
      new tdd.LogRecord(
      tdd.LogLevel.FAILURE,
      this.name_,
      this.currentTestMethodName_,
      /** @type {Object} */ (rejectionReason)));
};


/**
 * @param {string} name
 * @param {?} val
 * @return {boolean}
 * @private
 */
r5js.test.SyncPromiseTestSuite.isTestMethod_ = function(name, val) {
  return goog.isFunction(val) && goog.string.startsWith(name, 'test');
};


/**
 * @param {string} input
 * @param {!goog.Promise.<?>} promise
 * @return {!r5js.test.SyncPromiseTestSuite.Expectation_}
 * @protected
 */
r5js.test.SyncPromiseTestSuite.prototype.expect = function(input, promise) {
  var expectation = new r5js.test.SyncPromiseTestSuite.Expectation_(
      input, promise);
  this.expectationsPerTestMethod_[this.currentTestMethodName_].push(
      expectation);
  return expectation;
};



/**
 * @param {string} input
 * @param {!goog.Promise.<!r5js.JsonValue>} evalPromise
 * @struct
 * @constructor
 * @private
 */
r5js.test.SyncPromiseTestSuite.Expectation_ = function(input, evalPromise) {
  /** @const @private */ this.input_ = input;
  /** @const @private */ this.promise_ = evalPromise;
  /** @private {tdd.matchers.Matcher} */ this.matcher_ = null;
  /** @private */ this.invert_ = false;
};


/** @return {!r5js.test.SyncPromiseTestSuite.Expectation_} */
r5js.test.SyncPromiseTestSuite.Expectation_.prototype.not = function() {
  this.invert_ = !this.invert_;
  return this;
};


/** @param {!tdd.matchers.Matcher} matcher */
r5js.test.SyncPromiseTestSuite.Expectation_.prototype.to = function(matcher) {
  this.matcher_ = matcher;
};


/** @return {!goog.Promise.<?>} */
r5js.test.SyncPromiseTestSuite.Expectation_.prototype.getPromise = function() {
  return this.promise_.then(
      this.resolveOrReject_, this.resolveOrReject_, this);
};


/**
 * @param {?} valueOrReason
 * @return {!goog.Promise.<?>}
 * @private
 */
r5js.test.SyncPromiseTestSuite.Expectation_.prototype.resolveOrReject_ =
    function(valueOrReason) {
  var matches = this.matcher_.matches(valueOrReason);
  return ((this.invert_ && matches) || (!this.invert_ && !matches)) ?
      goog.Promise.reject(this.matcher_.getFailureMessage(valueOrReason)) :
      goog.Promise.resolve(null);
};
