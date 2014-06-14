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





goog.provide('r5js.platform.html5.MessageType');
goog.provide('r5js.platform.html5.message');



/**
 * Structure for messages passed between {@link r5js.platform.html5.Client}
 * and {@link r5js.platform.html5.Worker}. This type isn't goog.provided;
 * use the r5js.platform.html5.message.* helper functions instead.
 * @param {!r5js.platform.html5.MessageType} type Message type. Ordinarily,
 * type tags are inferior to subclassing. However, the HTML5 structured clone
 * algorithm restricts the data that can pass between a worker and its parent.
 * To avoid several pitfalls (for example, functions can't be serialized),
 * we stick to a simple structure and use type tags.
 * @param {number} id Message id.
 * @param {string} content Message content.
 * @struct
 * @constructor
 */
r5js.platform.html5.Message = function(type, id, content) {
  /** @const */ this.type = type;
  /** @const */ this.id = id;
  /** @const */ this.content = content;
};


/** @enum {number} */
r5js.platform.html5.MessageType = {
  EVAL_TO_STRING_REQUEST: 0,
  EVAL_TO_JS_REQUEST: 1,
  EVAL_RESPONSE: 2,
  EVAL_ERROR: 3,
  WRITE_VALUE: 4,
  WRITE_CHAR: 5,
  DISPLAY: 6
};


/**
 * @param {number} id Message id.
 * @param {string} request Message content.
 * @return {!r5js.platform.html5.Message}
 */
r5js.platform.html5.message.newEvalToStringRequest = function(id, request) {
  return new r5js.platform.html5.Message(
      r5js.platform.html5.MessageType.EVAL_TO_STRING_REQUEST, id, request);
};


/**
 * @param {number} id Message id.
 * @param {string} response Message content.
 * @return {!r5js.platform.html5.Message}
 */
r5js.platform.html5.message.newEvalResponse = function(id, response) {
  return new r5js.platform.html5.Message(
      r5js.platform.html5.MessageType.EVAL_RESPONSE, id, response);
};


/**
 * @param {number} id Message id.
 * @param {string} request Message content.
 * @return {!r5js.platform.html5.Message}
 */
r5js.platform.html5.message.newEvalToJsRequest = function(id, request) {
  return new r5js.platform.html5.Message(
      r5js.platform.html5.MessageType.EVAL_TO_JS_REQUEST, id, request);
};


/**
 * @param {number} id Message id.
 * @param {string} errorMsg Message content.
 * @return {!r5js.platform.html5.Message}
 */
r5js.platform.html5.message.newEvalError = function(id, errorMsg) {
  return new r5js.platform.html5.Message(
      r5js.platform.html5.MessageType.EVAL_ERROR, id, errorMsg);
};


/**
 * @param {!r5js.JsonValue} value
 * @return {!r5js.platform.html5.Message}
 */
r5js.platform.html5.message.writeValue = function(value) {
  return new r5js.platform.html5.Message(
      r5js.platform.html5.MessageType.WRITE_VALUE, -1,
      value.writeValue);
};


/**
 * @param {string} c
 * @return {!r5js.platform.html5.Message}
 */
r5js.platform.html5.message.writeChar = function(c) {
  return new r5js.platform.html5.Message(
      r5js.platform.html5.MessageType.WRITE_CHAR, -1, c);
};


/**
 * @param {!r5js.JsonValue} value
 * @return {!r5js.platform.html5.Message}
 */
r5js.platform.html5.message.display = function(value) {
  return new r5js.platform.html5.Message(
      r5js.platform.html5.MessageType.DISPLAY, -1,
      value.displayValue);
};








