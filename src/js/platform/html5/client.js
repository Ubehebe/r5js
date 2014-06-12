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
goog.require('r5js.platform.html5.MessageType');



/**
 * @param {string} scriptName
 * @implements {r5js.Evaluator}
 * @struct
 * @constructor
 */
r5js.platform.html5.Client = function(scriptName) {
  /** @const @private */ this.worker_ = new Worker(scriptName);

  this.worker_.addEventListener(
      goog.events.EventType.MESSAGE, this.onMessage_.bind(this), false);

  /** @private */ this.messageIdCounter_ = 0;

  /** @const @private {!Array.<function(*)>} */
  this.resolvers_ = [];

  /** @const @private {!Array.<function(*)>}*/
  this.rejecters_ = [];

  this.worker_.postMessage(r5js.platform.html5.message.boot());
};


/** @override */
r5js.platform.html5.Client.prototype.evaluate = function(input) {
  return new goog.Promise(function(resolve, reject) {
    var messageId = this.messageIdCounter_++;
    this.resolvers_[messageId] = resolve;
    this.rejecters_[messageId] = reject;
    this.worker_.postMessage(
        r5js.platform.html5.message.newEvalRequest(messageId, input));
  }, this);
};


/**
 * @param {!Event} e
 * @private
 */
r5js.platform.html5.Client.prototype.onMessage_ = function(e) {
  e = /** @type {!MessageEvent} */ (e);
  var message = /** @type {!r5js.platform.html5.Message} */ (e.data);
  switch (message.type) {
    case r5js.platform.html5.MessageType.EVAL_RESPONSE:
      this.resolvers_[message.id](message.content);
      break;
    case r5js.platform.html5.MessageType.EVAL_ERROR:
      this.rejecters_[message.id](message.content);
      break;
  }
  delete this.resolvers_[message.id];
  delete this.rejecters_[message.id];
};


/**
 * @param {!MessageEvent} e
 * @private
 */
r5js.platform.html5.Client.prototype.onError_ = function(e) {
  var message = /** @type {!r5js.platform.html5.Message} */ (e.data);
  var rejecter = this.rejecters_[message.id];
  delete this.resolvers_[message.id];
  delete this.rejecters_[message.id];
  if (rejecter) {
    rejecter(message.content);
  }
};
