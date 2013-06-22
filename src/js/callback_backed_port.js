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

/* We name the functions with string literals, not properties, to
 prevent the Google Closure Compiler from renaming them. See comments
 at Port. */


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
goog.exportSymbol(
    'r5js.CallbackBackedPort.prototype.close',
    r5js.CallbackBackedPort.prototype.close
);


/** @override */
r5js.CallbackBackedPort.prototype.peekChar = function() {
    return this; // = EOF
};
goog.exportSymbol(
    'r5js.CallbackBackedPort.prototype.peekChar',
    r5js.CallbackBackedPort.prototype.peekChar
);

/** @override */
r5js.CallbackBackedPort.prototype.readChar = function () {
    return this; // i.e. an EOF object
};
goog.exportSymbol(
    'r5js.CallbackBackedPort.prototype.readChar',
    r5js.CallbackBackedPort.prototype.readChar
);


/** @override */
r5js.CallbackBackedPort.prototype.isEof = function() {
    return true;
};
goog.exportSymbol(
    'r5js.CallbackBackedPort.prototype.isEof',
    r5js.CallbackBackedPort.prototype.isEof
);


/** @override */
r5js.CallbackBackedPort.prototype.isCharReady = function() {
    return true; // Because we're always at EOF
};
goog.exportSymbol(
    'r5js.CallbackBackedPort.prototype.isCharReady',
    r5js.CallbackBackedPort.prototype.isCharReady
);


/** @override */
r5js.CallbackBackedPort.prototype.toString = function() {
    return '[javascript]';
};
goog.exportSymbol(
    'r5js.CallbackBackedPort.prototype.toString',
    r5js.CallbackBackedPort.prototype.toString
);


/** @override */
r5js.CallbackBackedPort.prototype.writeChar = function(c) {
    this.onOutput_(c);
};
goog.exportSymbol(
    'r5js.CallbackBackedPort.prototype.writeChar',
    r5js.CallbackBackedPort.prototype.writeChar
);


r5js.CallbackBackedPort.prototype.write = function(str) {
    this.onOutput_(str);
};
goog.exportSymbol(
    'r5js.CallbackBackedPort.prototype.write',
    r5js.CallbackBackedPort.prototype.write
);