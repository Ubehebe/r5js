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

goog.provide('r5js.test.SchemeTestDriver');
goog.setTestOnly('r5js.test.SchemeTestDriver');


goog.require('expect');
goog.require('goog.Promise');
goog.require('r5js.CallbackBackedPort');
goog.require('r5js.Platform');
goog.require('r5js.valutil');
goog.require('tdd.LogLevel');
goog.require('tdd.LogRecord');
goog.require('tdd.ManualTestSuite');
goog.require('tdd.ResultStruct');



/**
 * Driver for running the unit tests written in Scheme.
 * @implements {tdd.ManualTestSuite}
 * @struct
 * @constructor
 */
r5js.test.SchemeTestDriver = function() {
  /** @private */ this.result_ = new tdd.ResultStruct(0, 0, 0);
  /** @private {goog.log.Logger} */ this.logger_ = null;
};
tdd.ManualTestSuite.addImplementation(r5js.test.SchemeTestDriver);


/** @override */
r5js.test.SchemeTestDriver.prototype.getType = function() {
  return tdd.TestType.UNIT;
};


/** @override */
r5js.test.SchemeTestDriver.prototype.toString = function() {
  return 'r5js.test.SchemeTestDriver';
};


/** @override */
r5js.test.SchemeTestDriver.prototype.estimateSize = function() {
  return 52; // TODO bl how to do this without evaluating?
};


/** @override */
r5js.test.SchemeTestDriver.prototype.execute = function(logger) {
  this.logger_ = logger;
  var platform = r5js.Platform.get();
  var result = this.result_;
  var onWrite = this.onWrite_.bind(this);
  /** @type {r5js.test.SchemeSources} */ var sources;
  /** @type {r5js.Evaluator} */ var evaluator;

  return goog.Promise.all([
    platform.getTestSources(),
    platform.newEvaluator(r5js.InputPort.NULL,
        new r5js.CallbackBackedPort(onWrite))]).then(function(resolved) {
    sources = resolved[0];
    evaluator = resolved[1];
  }).then(function() {
    return new r5js.test.SchemeTestDriver.TestFrameworkTest_(
        /** @type {!r5js.test.SchemeSources} */ (sources)).execute(logger);
  }).then(function(result_) {
    result = result.merge(result_);
    return evaluator.evaluate(
        sources.testFramework + sources.r5RSTests);
  }).then(function() {
    return evaluator.evaluate(
        sources.testFramework + sources.negativeTests);
  }).then(function() {
    return evaluator.evaluate(
        sources.testFramework + sources.otherTests);
  }).then(function() {
    return this.result_;
  }, undefined /* opt_onRejected */, this);
};


/**
 * @param {string} str
 * @private
 */
r5js.test.SchemeTestDriver.prototype.onWrite_ = function(str) {
  var result = r5js.test.SchemeTestDriver.stringToResultStruct_(str);
  if (result) {
    this.logger_.logRecord(new tdd.LogRecord(
        result.getNumFailed() ? tdd.LogLevel.FAILURE : tdd.LogLevel.SUCCESS,
        'r5js.test.SchemeTestDriver',
        result.name_));
    this.result_ = this.result_.merge(result);
  } else if (result = r5js.test.SchemeTestDriver.stringToFailureMessage_(
      str)) {
    this.logger_.logRecord(new tdd.LogRecord(
        tdd.LogLevel.FAILURE,
        'r5js.test.SchemeTestDriver',
        result));
  }
};



/**
 * @param {string} name
 * @param {number} numSucceeded
 * @param {number} numFailed
 * @extends {tdd.ResultStruct}
 * @struct
 * @constructor
 * @private
 */
r5js.test.SchemeTestDriver.ResultStruct_ = function(
    name, numSucceeded, numFailed) {
  r5js.test.SchemeTestDriver.ResultStruct_.base(
      this, 'constructor', numSucceeded, numFailed, 0 /* TODO bl */);
  /** @const @private */ this.name_ = name;
};
goog.inherits(r5js.test.SchemeTestDriver.ResultStruct_, tdd.ResultStruct);


/**
 * Parses a Scheme test framework output like this:
 * (foo-tests (3 tests) (1 failed))
 * into a {@link tdd.ResultStruct}, returning null if the parse failed.
* @param {string} str
 * @return {r5js.test.SchemeTestDriver.ResultStruct_}
 * @private
 */
r5js.test.SchemeTestDriver.stringToResultStruct_ = function(str) {
  var regex = /\((.+) \((\d+) tests\) \((\d+) failed\)\)/;
  var matches = regex.exec(str);
  if (!matches) {
    return null;
  }
  var name = matches[1];
  var numSucceeded = parseInt(matches[2], 10);
  var numFailed = parseInt(matches[3], 10);
  return new r5js.test.SchemeTestDriver.ResultStruct_(
      name, numSucceeded, numFailed);
};


/**
 * Parses a Scheme test framework output like this:
 * (fail foo-tests (input (+ 1 1)) (want 3) (got 2))
 * into a string, returning null if the parse failed.
 * @param {string} str
 * @return {?string}
 * @private
 */
r5js.test.SchemeTestDriver.stringToFailureMessage_ = function(str) {
  var match = /\(fail .+ \(input (.*)\) \(want (.*)\) \(got (.*)\)\)/.
      exec(str);
  if (!match) {
    return null;
  }
  var input = match[1];
  var want = match[2];
  var got = match[3];
  return 'input ' + input + ': want ' + want + ', got ' + got;
};



/**
 * @param {!r5js.test.SchemeSources} sources
 * @implements {tdd.ManualTestSuite}
 * @struct
 * @constructor
 * @private
 */
r5js.test.SchemeTestDriver.TestFrameworkTest_ = function(sources) {
  /** @const @private */ this.sources_ = sources;
  /** @private */
  this.actualResult_ = new r5js.test.SchemeTestDriver.ResultStruct_('', 0, 0);

  /** @private {goog.log.Logger} */ this.logger_ = null;
};
tdd.ManualTestSuite.addImplementation(
    r5js.test.SchemeTestDriver.TestFrameworkTest_);


/**
 * Must be kept manually in sync with the expected results of
 * test/unit-test-tests.scm.
 * @param {!tdd.ResultStruct} result
 * @return {boolean}
 * @private
 */
r5js.test.SchemeTestDriver.TestFrameworkTest_.resultIsExpected_ =
    function(result) {
  return result.getNumRun() === 3 &&
      result.getNumSucceeded() === 2 &&
      result.getNumFailed() === 1;
};


/**
 * @param {string} str
 * @private
 */
r5js.test.SchemeTestDriver.TestFrameworkTest_.prototype.onWrite_ = function(
    str) {
  var result = r5js.test.SchemeTestDriver.stringToResultStruct_(str);
  if (result) {
    this.logger_.logRecord(new tdd.LogRecord(
        tdd.LogLevel.SUCCESS,
        'r5js.test.SchemeTestDriver',
        result.name_));
    this.actualResult_ = this.actualResult_.merge(result);
  }
};


/** @override */
r5js.test.SchemeTestDriver.TestFrameworkTest_.prototype.getType =
    goog.functions.constant(tdd.TestType.UNIT);


/** @override */
r5js.test.SchemeTestDriver.TestFrameworkTest_.prototype.estimateSize =
    goog.functions.constant(1);


/** @override */
r5js.test.SchemeTestDriver.TestFrameworkTest_.prototype.toString =
    goog.functions.constant('r5js.test.SchemeTestDriver.TestFrameworkTest_');


/** @override */
r5js.test.SchemeTestDriver.TestFrameworkTest_.prototype.execute =
    function(logger) {
  this.logger_ = logger;
  return r5js.Platform.get().newEvaluator(
      r5js.InputPort.NULL,
      new r5js.CallbackBackedPort(this.onWrite_.bind(this)))
      .then(function(evaluator) {
        return evaluator.evaluate(
           this.sources_.testFramework + this.sources_.testFrameworkTests)
      .then(function() {
             return r5js.test.SchemeTestDriver.TestFrameworkTest_.
             resultIsExpected_(this.actualResult_);
           }, undefined /* opt_onRejected */, this)
      .then(function(success) {
             return goog.Promise.resolve(
             success ?
             new tdd.ResultStruct(1, 0, 0) :
             new tdd.ResultStruct(0, 1, 0));
           });
      }, undefined /* opt_onRejected */, this);
};
