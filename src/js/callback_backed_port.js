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


goog.provide('r5js.tmp.callback_backed_port');

function CallbackBackedPort(onOutput) {
    if (typeof onOutput !== 'function')
        throw new InternalInterpreterError('invariant incorrect');
    this.onOutput = onOutput;
}

CallbackBackedPort.prototype['close'] = function() {/* no-op */ };

CallbackBackedPort.prototype['peekChar'] = function() {
    return this; // = EOF
};

CallbackBackedPort.prototype['readChar'] = function () {
    return this; // i.e. an EOF object
};

CallbackBackedPort.prototype['isEof'] = function() {
    return true;
};

CallbackBackedPort.prototype['isCharReady'] = function() {
    return true; // Because we're always at EOF
};

CallbackBackedPort.prototype['toString'] = function() {
    return '[javascript]';
};

CallbackBackedPort.prototype['writeChar'] = function(c) {
    this.onOutput(c);
};

CallbackBackedPort.prototype['write'] = function(str) {
    this.onOutput(str);
};
