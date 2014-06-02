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

/**
 * @fileoverview Convenience functions for running tests behind a web worker.
 */
goog.provide('r5js.test.WorkerDriver');
goog.setTestOnly('r5js.test.WorkerDriver');


goog.require('goog.debug.Logger.Level');
goog.require('goog.events.EventType');
goog.require('goog.log.Level');
goog.require('goog.log.LogRecord');
goog.require('tdd.logTo');


/** @private {!tdd.LogWriter|null} */
r5js.test.WorkerDriver.logWriter_ = null;


/** @private {Worker} */
r5js.test.WorkerDriver.worker_ = null;


/**
 * @return {!tdd.LogWriter}
 * @private
 */
r5js.test.WorkerDriver.getLogWriter_ = function() {
  if (!r5js.test.WorkerDriver.logWriter_) {
    r5js.test.WorkerDriver.logWriter_ = tdd.logTo(console);
  }
  return r5js.test.WorkerDriver.logWriter_;
};


/**
 * @return {!Worker}
 * @private
 */
r5js.test.WorkerDriver.getWorker_ = function() {
  if (!r5js.test.WorkerDriver.worker_) {
    r5js.test.WorkerDriver.worker_ = new Worker('../src/js/test/worker.js');
    r5js.test.WorkerDriver.worker_.addEventListener(
        goog.events.EventType.MESSAGE,
        r5js.test.WorkerDriver.onMessage_,
        false /* opt_useCapture */);
  }
  return r5js.test.WorkerDriver.worker_;
};


/**
 * @param {!Event} e
 * @private
 * @suppress {accessControls|checkTypes} for reconstituting the LogRecord
 * TODO bl also reconstitute the error, if any.
 */
r5js.test.WorkerDriver.onMessage_ = function(e) {
  var logRecord = /** @type {!goog.log.LogRecord} */ (JSON.parse(e.data));
  var reconstitutedLevel = goog.debug.Logger.Level.getPredefinedLevel(
      logRecord.level_.name);
  var reconstitutedLogRecord = new goog.log.LogRecord(
      reconstitutedLevel,
      logRecord.msg_,
      logRecord.loggerName_);
  r5js.test.WorkerDriver.getLogWriter_()(reconstitutedLogRecord);
};


/** Calls {@link r5js.test.main} through the worker. */
r5js.test.WorkerDriver.main = function() {
  r5js.test.WorkerDriver.getWorker_().postMessage({name: 'r5js.test.main'});
};


/**
 * Calls {@link r5js.test.readSandbox} through the worker.
 * @param {string} text Text to read.
 */
r5js.test.WorkerDriver.readSandbox = function(text) {
  r5js.test.WorkerDriver.getWorker_().postMessage({
    name: 'r5js.test.readSandbox',
    args: [text]
  });
};


/**
 * Calls {@link r5js.test.parseSandbox} through the worker.
 * @param {string} text Text to parse.
 */
r5js.test.WorkerDriver.parseSandbox = function(text) {
  r5js.test.WorkerDriver.getWorker_().postMessage({
    name: 'r5js.test.parseSandbox',
    args: [text]
  });
};


/**
 * Calls {@link r5js.test.evalSandbox} through the worker.
 * @param {string} text Text to evaluate.
 */
r5js.test.WorkerDriver.evalSandbox = function(text) {
  r5js.test.WorkerDriver.getWorker_().postMessage({
    name: 'r5js.test.evalSandbox',
    args: [text]
  });
};


