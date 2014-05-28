/**
 * @fileoverview Convenience functions for running tests behind a web worker.
 */
goog.provide('r5js.test.WorkerDriver');
goog.setTestOnly('r5js.test.WorkerDriver');


goog.require('goog.events.EventType');


/** @private {Worker} */
r5js.test.WorkerDriver.worker_ = null;


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
 */
r5js.test.WorkerDriver.onMessage_ = function(e) {
  console.log(e.data);
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


