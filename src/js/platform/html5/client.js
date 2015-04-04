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


goog.provide('r5js.platform.html5.Client');


goog.require('goog.Promise');
goog.require('goog.events.EventType');
goog.require('r5js.Evaluator');
goog.require('r5js.platform.html5.MessageType');



/**
 * @param {!r5js.OutputPort} outputPort
 * @implements {r5js.Evaluator}
 * @struct
 * @constructor
 */
r5js.platform.html5.Client = function(outputPort) {
  /** @const @private */ this.worker_ =
      new Worker(r5js.platform.html5.Client.WORKER_SCRIPT);

  /** @const @private */ this.outputPort_ = outputPort;

  this.worker_.addEventListener(
      goog.events.EventType.MESSAGE, this.onMessage_.bind(this), false);

  /** @private */ this.messageIdCounter_ = 0;

  /** @const @private {!Array<!goog.promise.Resolver<?>>} */
  this.resolvers_ = [];
};


/** @define {string} Location of the worker script. */
goog.define('r5js.platform.html5.Client.WORKER_SCRIPT', '');


/** @override */
r5js.platform.html5.Client.prototype.evaluate = function(input) {
  const resolver = goog.Promise.withResolver();
  const messageId = ++this.messageIdCounter_;
  this.resolvers_[messageId] = resolver;
  this.worker_.postMessage(
      r5js.platform.html5.message.newEvalRequest(messageId, input));
  return resolver.promise;
};


/**
 * @param {!Event} e
 * @private
 */
r5js.platform.html5.Client.prototype.onMessage_ = function(e) {
  e = /** @type {!MessageEvent} */ (e);
  const message = /** @type {!r5js.platform.html5.Message} */ (e.data);
  switch (message.type) {
    case r5js.platform.html5.MessageType.EVAL_RESPONSE:
      this.resolvers_[message.id].resolve(message.content);
      break;
    case r5js.platform.html5.MessageType.EVAL_ERROR:
      this.resolvers_[message.id].reject(message.content);
      break;
    case r5js.platform.html5.MessageType.OUTPUT:
      this.outputPort_.write(/** @type {string} */ (message.content));
      return;
  }
  delete this.resolvers_[message.id];
};
