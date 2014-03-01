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


goog.provide('r5js.CallbackBackedPort');


goog.require('r5js.InternalInterpreterError');

/**
 * @param {Function} onOutput Callback that will be called whenever output
 * is available.
 * @implements {r5js.Port}
 * @constructor
 */
r5js.CallbackBackedPort = function(onOutput) {
    /**
     * @type {Function}
     * @private
     */
    this.onOutput_ = onOutput;
};


/** @override */
r5js.CallbackBackedPort.prototype.close = function() {};


/** @override */
r5js.CallbackBackedPort.prototype.peekChar = function() {
    return this; // = EOF
};

/** @override */
r5js.CallbackBackedPort.prototype.readChar = function () {
    return this; // i.e. an EOF object
};


/** @override */
r5js.CallbackBackedPort.prototype.isEof = function() {
    return true;
};


/** @override */
r5js.CallbackBackedPort.prototype.isCharReady = function() {
    return true; // Because we're always at EOF
};


/** @override */
r5js.CallbackBackedPort.prototype.toString = function() {
    return '[javascript]';
};


/** @override */
r5js.CallbackBackedPort.prototype.writeChar = function(c) {
    this.onOutput_(c);
};


r5js.CallbackBackedPort.prototype.write = function(str) {
    this.onOutput_(str);
};