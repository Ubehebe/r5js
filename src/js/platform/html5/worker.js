/**
 * @fileoverview Driver to run the tests from inside a web worker.
 * @suppress {undefinedVars} due to the unusual web worker setup.
 */
goog.module('r5js.platform.html5.Worker');

const boot = goog.require('r5js.boot');
const EventType = goog.require('goog.events.EventType');
const InputPort = goog.require('r5js.InputPort');
const Message = goog.require('r5js.platform.html5.Message');
const MessageType = goog.require('r5js.platform.html5.MessageType');
const OutputPort = goog.require('r5js.platform.html5.OutputPort');
const Promise = goog.require('goog.Promise');
const SchemeSources = goog.require('r5js.SchemeSources');
const SyncEvaluator = goog.require('r5js.sync.Evaluator');

/** Main entry point for the HTML5 worker. */
function worker() {
  addEventListener(EventType.MESSAGE, function(e) {
    const message = /** @type {!Message} */ (e.data);
    switch (message.type) {
      case MessageType.EVAL_REQUEST:
        handleEvalRequest(message);
        break;
    }
  }, false);
}

/** @type {SyncEvaluator} */ let evaluator = null;

/** @param {!Message} request */
function handleEvalRequest(request) {
    const evaluator = getEvaluator();
    try {
        const value = evaluator.evaluate(/** @type {string} */ (request.content));
        postMessage(Message.newEvalResponse(request.id, value));
    } catch (e) {
        postMessage(Message.newEvalError(request.id, e));
    }
}

/** @return {!SyncEvaluator} */
function getEvaluator() {
    if (!evaluator) {
        const inputPort = InputPort.NULL;
        const outputPort = new OutputPort(value => postMessage(Message.output(value)));
        const sources = SchemeSources.get();
        evaluator = boot(sources.syntax, sources.procedures, inputPort, outputPort);
    }
    return evaluator;
}

// Invoke the entry point immediately. This script is running in a worker.
worker();

