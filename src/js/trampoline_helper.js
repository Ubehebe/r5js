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

function TrampolineHelper(inputPort, outputPort) {
    this.inputPort = inputPort
        ? newInputPortDatum(inputPort)
        : this.discardInputPort;
    this.outputPort = outputPort
        ? newOutputPortDatum(outputPort)
        : this.discardOutputPort;

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

TrampolineHelper.prototype.discardInputPort = newInputPortDatum(
    new CallbackBackedPort(
        function() {
        }));

TrampolineHelper.prototype.discardOutputPort = newOutputPortDatum(
    new CallbackBackedPort(
        function() {
        }));