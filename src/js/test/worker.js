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
 * @fileoverview Driver to run the (uncompiled) tests from inside a web worker.
 * @suppress {undefinedVars|globalThis} due to the unusual web worker setup.
 */


// See bootstrap/webworkers.js in the Closure Library for discussion.
CLOSURE_BASE_PATH = '../../../closure-library/closure/goog/';
importScripts(
    CLOSURE_BASE_PATH + 'bootstrap/webworkers.js',
    CLOSURE_BASE_PATH + 'base.js',
    '../../../build/deps.js');

// TODO bl: nothing goog.requires this name, but typechecking appears
// not to work for this file unless it has a goog.provide.
goog.provide('r5js.test.Worker');
goog.setTestOnly('r5js.test.Worker');


goog.require('goog.events.EventType');
goog.require('r5js.test.main1');
goog.require('r5js.test.evalSandbox');
goog.require('r5js.test.parseSandbox');
goog.require('r5js.test.readSandbox');
goog.require('tdd.RunnerConfig');
goog.require('tdd.TestType');


/**
 * Serialize the log record to JSON and post it.
 * The record cannot be posted directly as an object, because
 * records representing failures have a field of type Error, and Error
 * objects explicitly cannot be serialized by the HTML5 structured
 * clone algorithm.
 * TODO bl: investigate why this is a silent error rather than a DataCloneError
 * exception, as required by the spec.
 * @param {!goog.log.LogRecord} logRecord
 * @private
 */
r5js.test.Worker.postLogRecord_ = function(logRecord) {
  postMessage(JSON.stringify(logRecord));
};


addEventListener(goog.events.EventType.MESSAGE, function(e) {
  var arg = e.data.args && e.data.args[0];
  switch (e.data.name) {
    case 'r5js.test.main':
      var config = new tdd.RunnerConfig()
          .setTestTypesToRun([tdd.TestType.UNIT, tdd.TestType.INTEGRATION])
          .addFailureHandler(r5js.test.Worker.postLogRecord_)
          .addSuccessHandler(r5js.test.Worker.postLogRecord_);
      r5js.test.main1(config);
      break;
    case 'r5js.test.readSandbox':
      r5js.test.readSandbox(arg);
      break;
    case 'r5js.test.parseSandbox':
      r5js.test.parseSandbox(arg);
      break;
    case 'r5js.test.evalSandbox':
      r5js.test.evalSandbox(arg);
      break;
    default:
      break;
  }
}, false /* opt_onCapture */);
