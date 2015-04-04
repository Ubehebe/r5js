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
goog.require('tdd.TestType');



/**
 * {@link tdd.TestSuite} implementation for writing Promise-based unit tests
 * with a synchronous syntax. Use with caution.
 *
 * When writing tests for asynchronous code, it is usually essential to deal
 * with the asynchronicity up-front. For example, it is essential to exercise
 * both the resolution and rejection branches for Promise-based APIs.
 *
 * There is at least one counterexample: when the APIs are asynchronous
 * but the tests exercise behavior for which asynchronicity is not an essential
 * concern.
 *
 * For example, {@link r5js.Evaluator} is a Promise-based API, but the
 * tests in {@link r5js.test.JsInterop} only deal with one evaluation at a time,
 * checking (hundreds of times) that a certain string evaluates to an expected
 * value. The relationship between one run of the evaluator and the next
 * is not under test.
 *
 * It would be possible to write these tests with a normal {@link tdd.TestSuite}
 * implementation, manually managing the (long) chain of Promises and
 * returning the last one to the test framework. But that would diminish
 * the tests' readability. It would also be possible to use a special
 * synchronous version of the API just for testing
 * (see {@link r5js.sync.Evaluator}), then the tests would no longer cover
 * the production API directly.
 *
 * Instead, this class allows test authors to write straight-line code
 * through the use of {@link #expect}. Test methods are run synchronously
 * at first to collect the Promises created by {@link #expect}, and then the
 * promise chains are kicked off.
 *
 * @param {string} name Name of the test suite.
 * @implements {tdd.ManualTestSuite}
 * @constructor
 */
r5js.test.SyncPromiseTestSuite = function(name) {

  /** @const @private */ this.name_ = name;
  /** @private {goog.log.Logger} */ this.logger_ = null;
  /** @const @private {!Array<!r5js.test.SyncPromiseTestSuite.Method_>} */
  this.testMethods_ = [];

  /** @private */ this.numSucceeded_ = 0;
  /** @private */ this.numFailed_ = 0;
  /** @private */ this.numExceptions_ = 0;
};
tdd.ManualTestSuite.addImplementation(r5js.test.SyncPromiseTestSuite);


/** @override */
r5js.test.SyncPromiseTestSuite.prototype.getType = goog.functions.constant(
    tdd.TestType.UNIT);


/** @override */
r5js.test.SyncPromiseTestSuite.prototype.estimateSize = function() {
  return 0; // TODO bl better estimate
};


/** @override */
r5js.test.SyncPromiseTestSuite.prototype.toString = function() {
  return this.name_;
};


/** @override */
r5js.test.SyncPromiseTestSuite.prototype.execute = function(logger) {
  this.logger_ = logger;
  for (const key in this) {
    const method = this[key];
    if (r5js.test.SyncPromiseTestSuite.isTestMethod_(key, method)) {
      this.testMethods_.push(new r5js.test.SyncPromiseTestSuite.Method_(
          key, method, this /* TODO bl remove */));
      // Call the test method synchronously to collect the promises.
      method.call(this);
    }
  }
  return this.runNextTestMethod_();
};


/**
 * @param {boolean} success
 * @private
 */
r5js.test.SyncPromiseTestSuite.prototype.reportOutcome_ = function(success) {
  if (success) {
    ++this.numSucceeded_;
  } else {
    ++this.numFailed_;
  }
};


/**
 * @return {!goog.Promise<!tdd.ResultStruct>}
 * @private
 */
r5js.test.SyncPromiseTestSuite.prototype.runNextTestMethod_ = function() {
  const testMethod = this.testMethods_.shift();
  return testMethod ?
      testMethod.runNextExpectation() :
      goog.Promise.resolve(
      new tdd.ResultStruct(
      this.numSucceeded_, this.numFailed_, this.numExceptions_));
};


/**
 * @return {!r5js.test.SyncPromiseTestSuite.Method_}
 * @private
 */
r5js.test.SyncPromiseTestSuite.prototype.getTestMethodUnderConstruction_ =
    function() {
  return this.testMethods_[this.testMethods_.length - 1];
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
 * @param {!goog.Promise<?>} promise
 * @return {!r5js.test.SyncPromiseTestSuite.Expectation_}
 * @protected
 */
r5js.test.SyncPromiseTestSuite.prototype.expect = function(input, promise) {
  /* TODO bl: without the goog.functions.identity callback,
     rejected evaluation promises end up invoking goog.promise's
     unhandled rejection handler. By default, the unhandled rejection handler
     throws an exception on the next tick, which is fine for browsers
     (it ends up on the console) but terminates execution for Node.

     After auditing all my goog.Promise#then call sites, I haven't been able
     to figure out why the unhandled rejection handler is getting called.
     It seems that goog.promise.removeUnhandledRejection_ is getting called
     after goog.promise.addUnhandledRejection_, by which time it is too late. */
  const expectation = new r5js.test.SyncPromiseTestSuite.Expectation_(
      input, promise.thenCatch(goog.functions.identity));
  this.getTestMethodUnderConstruction_().addExpectation(expectation);
  return expectation;
};



/**
 * @param {string} input
 * @param {!goog.Promise<string>} evalPromise
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


/** @return {!goog.Promise<?>} */
r5js.test.SyncPromiseTestSuite.Expectation_.prototype.getPromise = function() {
  return this.promise_.then(
      this.resolveOrReject_, this.resolveOrReject_, this);
};


/**
 * @param {?} valueOrReason
 * @return {!goog.Promise<?>}
 * @private
 */
r5js.test.SyncPromiseTestSuite.Expectation_.prototype.resolveOrReject_ =
    function(valueOrReason) {
  const matches = this.matcher_.matches(valueOrReason);
  return ((this.invert_ && matches) || (!this.invert_ && !matches)) ?
      goog.Promise.reject(
          this.input_ + ': ' + this.matcher_.getFailureMessage(valueOrReason)) :
      goog.Promise.resolve(null);
};



/**
 * @param {string} name The test method's name.
 * @param {!Function} func The test method.
 * @param {!r5js.test.SyncPromiseTestSuite} parent TODO bl remove.
 * @struct
 * @constructor
 * @private
 */
r5js.test.SyncPromiseTestSuite.Method_ = function(name, func, parent) {
  /** @const @private */ this.name_ = name;
  /** @const @private */ this.func_ = func;
  /** @const @private */ this.parent_ = parent;
  /**
     * @private {!Array<!r5js.test.SyncPromiseTestSuite.Expectation_>}
     * @const
     */
  this.expectations_ = [];

  /** @private */ this.success_ = true;
};


/** @param {!r5js.test.SyncPromiseTestSuite.Expectation_} expectation */
r5js.test.SyncPromiseTestSuite.Method_.prototype.addExpectation = function(
    expectation) {
  this.expectations_.push(expectation);
};


/** @return {!goog.Promise<?>} */
r5js.test.SyncPromiseTestSuite.Method_.prototype.runNextExpectation =
    function() {
  const expectation = this.expectations_.shift();
  if (expectation) {
    return expectation.getPromise().
        thenCatch(this.onRejected, this).
        then(this.runNextExpectation, this.runNextExpectation, this);
  } else {
    this.parent_.reportOutcome_(this.success_);
    if (this.success_) {
      this.parent_.logger_.logRecord(
          new tdd.LogRecord(
          tdd.LogLevel.SUCCESS,
          this.parent_.name_,
          this.name_));
    }
    return this.parent_.runNextTestMethod_();
  }
};


/** @param {*} rejectionReason */
r5js.test.SyncPromiseTestSuite.Method_.prototype.onRejected = function(
    rejectionReason) {
  this.success_ = false;
  this.parent_.logger_.logRecord(
      new tdd.LogRecord(
      tdd.LogLevel.FAILURE,
      this.parent_.name_,
      this.name_,
      /** @type {Object} */ (rejectionReason)));
};
