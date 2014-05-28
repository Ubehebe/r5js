/**
 * @fileoverview Driver to run the (uncompiled) tests from inside a web worker.
 * @suppress {undefinedVars,globalThis} due to the unusual web worker setup.
 */


// See bootstrap/webworkers.js in the Closure Library for discussion.
CLOSURE_BASE_PATH = '../../../closure-library/closure/goog/';
importScripts(
    CLOSURE_BASE_PATH + 'bootstrap/webworkers.js',
    CLOSURE_BASE_PATH + 'base.js',
    CLOSURE_BASE_PATH + 'deps.js',
    '../../../build/deps.js');

// TODO bl: goog.labs.net.xhr calls window.clearTimeout directly.
// Use this polyfill until it is fixed.
window = this.window || {clearTimeout: function() {}};

// TODO bl: nothing goog.requires this name, but typechecking appears
// not to work for this file unless it has a goog.provide.
goog.provide('r5js.test.Worker');


goog.require('r5js.test.main1');
goog.require('r5js.test.evalSandbox');
goog.require('r5js.test.parseSandbox');
goog.require('r5js.test.readSandbox');
goog.require('tdd.Formatter');
goog.require('tdd.RunnerConfig');
goog.require('tdd.TestType');


addEventListener('message', function(e) {
  var arg = e.data.args && e.data.args[0];
  switch (e.data.name) {
    case 'r5js.test.main':
      var formatter = new tdd.Formatter();
      var config = new tdd.RunnerConfig().
          setTestTypesToRun([tdd.TestType.UNIT, tdd.TestType.INTEGRATION
          ]).addFailureHandler(function(logRecord) {
                postMessage(formatter.formatRecord(logRecord));
          }).addSuccessHandler(function(logRecord) {
                postMessage(formatter.formatRecord(logRecord));
          });
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
});