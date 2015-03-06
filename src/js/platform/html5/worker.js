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
 * @fileoverview Driver to run the tests from inside a web worker.
 * @suppress {undefinedVars|globalThis} due to the unusual web worker setup.
 */


goog.provide('r5js.platform.html5.Worker');


goog.require('goog.events.EventType');
goog.require('r5js.InputPort');
goog.require('r5js.SchemeSources');
goog.require('r5js.boot');
goog.require('r5js.curPlatform');
goog.require('r5js.platform.html5.MessageType');
goog.require('r5js.platform.html5.OutputPort');
goog.require('r5js.valutil');


/**
 * Main entry point for the HTML5 worker.
 */
r5js.platform.html5.Worker = function() {
  addEventListener(goog.events.EventType.MESSAGE, function(e) {
    var message = /** @type {!r5js.platform.html5.Message} */ (e.data);
    switch (message.type) {
      case r5js.platform.html5.MessageType.EVAL_REQUEST:
        r5js.platform.html5.Worker.handleEvalRequest_(message);
        break;
    }
  }, false);
};


/** @private {goog.Promise<!r5js.sync.Evaluator>} */
r5js.platform.html5.Worker.evaluator_;


/**
 * @param {!r5js.platform.html5.Message} message
 * @private
 * @suppress {checkTypes} TODO bl for newEvalToStringResponse
 */
r5js.platform.html5.Worker.handleEvalRequest_ = function(message) {
  r5js.platform.html5.Worker.getEvaluator_().then(function(evaluator) {
    /** @type {string} */ var value;
    try {
      value = evaluator.evaluate(message.content);
    } catch (e) {
      postMessage(
          r5js.platform.html5.message.newEvalError(message.id, e));
      return;
    }
    postMessage(
        r5js.platform.html5.message.newEvalResponse(message.id, value));
  });
};


/**
 * @return {!goog.Promise<!r5js.sync.Evaluator>}
 * @private
 */
r5js.platform.html5.Worker.getEvaluator_ = function() {
  if (!r5js.platform.html5.Worker.evaluator_) {
    var inputPort = r5js.InputPort.NULL;
    var outputPort = new r5js.platform.html5.OutputPort(function(value) {
      postMessage(r5js.platform.html5.message.output(value));
    });
    var platform = r5js.curPlatform();
    r5js.platform.html5.Worker.evaluator_ = r5js.SchemeSources.get().
        then(function(sources) {
              return r5js.boot(
                  sources.syntax,
                  sources.procedures,
                  inputPort,
                  outputPort);
        });
  }
  return r5js.platform.html5.Worker.evaluator_;
};


// Invoke the entry point immediately. This script is running in a worker.
r5js.platform.html5.Worker();

