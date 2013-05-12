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


goog.provide('r5js.tmp.trampoline_helper');


goog.require('r5js.tmp.callback_backed_port');

/**
 * @constructor
 */
function TrampolineHelper(inputPort, outputPort) {
    this.inputPort = inputPort
        ? newInputPortDatum(inputPort)
        : this.discardInputPort_();
    this.outputPort = outputPort
        ? newOutputPortDatum(outputPort)
        : this.discardOutputPort_();

    /*
     this.ans;
     this.nextContinuable;
     this.beforeThunk;
     */
}

TrampolineHelper.prototype.clear = function() {
    this.ans = null;
    this.nextContinuable = null;
};

/**
 * @return {!Datum} New Datum representing a discard input port.
 * @private
 */
TrampolineHelper.prototype.discardInputPort_ = function() {
    return newInputPortDatum(
        new CallbackBackedPort(
            function() {
            }));
};

/**
 * @return {!Datum} New Datum representing a discard output port.
 * @private
 */
TrampolineHelper.prototype.discardOutputPort_ = function() {
    return newOutputPortDatum(
        new CallbackBackedPort(
            function() {
            }));
};