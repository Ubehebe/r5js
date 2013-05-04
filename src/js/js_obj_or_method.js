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


goog.provide('r5js.tmp.js_obj_or_method');


goog.require('r5js.InternalInterpreterError');

/* It's kind of silly to have an object representing an object.
 I did this to avoid dispatching on typeof x === 'object' in the
 evaluator, which in my experience is error-prone. */

/**
 * @constructor
 */
function JsObjOrMethod(receiver, msg /* null for obj */) {
    this.receiver = receiver;
    this.msg = msg;
}

JsObjOrMethod.prototype.isBoundMethod = function() {
    return !!this.msg;
};

JsObjOrMethod.prototype.getObject = function() {
    if (this.isBoundMethod())
        throw new r5js.InternalInterpreterError('invariant incorrect');
    return this.receiver;
};

JsObjOrMethod.prototype.callWith = function(args) {
    return this.msg.apply(this.receiver, args);
};

JsObjOrMethod.prototype.toString = function() {
    return this.receiver.toString();
};