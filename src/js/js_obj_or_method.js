/* Copyright 2011, 2012 Brendan Linn

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


goog.provide('r5js.JsObjOrMethod');


goog.require('r5js.InternalInterpreterError');



/**
 * @param {*} receiver The receiver.
 * @param {*=} opt_msg The message to send.
 * @struct
 * @constructor
 * TODO bl: narrow the types of the parameters.
 */
r5js.JsObjOrMethod = function(receiver, opt_msg) {
  /** @const @private */ this.receiver_ = receiver;
  /** @const @private */ this.msg_ = opt_msg;
};


/** @return {boolean} True iff the object represents a bound method. */
r5js.JsObjOrMethod.prototype.isBoundMethod = function() {
  return !!this.msg_;
};


/** @return {*} */
r5js.JsObjOrMethod.prototype.getObject = function() {
  if (this.isBoundMethod()) {
    throw new r5js.InternalInterpreterError('invariant incorrect');
  }
  return this.receiver_;
};


/**
 * @param {*} args TODO bl document.
 * @return {*} TODO bl.
 */
r5js.JsObjOrMethod.prototype.callWith = function(args) {
  return this.msg_.apply(this.receiver_, args);
};


/** @override */
r5js.JsObjOrMethod.prototype.toString = function() {
  return this.receiver_.toString();
};
