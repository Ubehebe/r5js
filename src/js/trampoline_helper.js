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


goog.provide('r5js.TrampolineHelper');


goog.require('r5js.tmp.callback_backed_port');

/**
 * @constructor
 */
r5js.TrampolineHelper = function(inputPort, outputPort) {
    /**
     * @type {!Datum}
     * @private
     */
    this.inputPort_ = inputPort ?
        newInputPortDatum(inputPort) :
        this.discardInputPort_();

    /**
     * @type {!Datum}
     * @private
     */
    this.outputPort_ = outputPort ?
        newOutputPortDatum(outputPort) :
        this.discardOutputPort_();

    /*
     this.ans;
     this.nextContinuable;
     this.beforeThunk;
     */
};

r5js.TrampolineHelper.prototype.clear = function() {
    this.ans = null;
    this.nextContinuable = null;
};

/**
 * @return {!Datum} New Datum representing a discard input port.
 * @private
 */
r5js.TrampolineHelper.prototype.discardInputPort_ = function() {
    return newInputPortDatum(
        new CallbackBackedPort(
            function() {
            }));
};

/**
 * @return {!Datum} New Datum representing a discard output port.
 * @private
 */
r5js.TrampolineHelper.prototype.discardOutputPort_ = function() {
    return newOutputPortDatum(
        new CallbackBackedPort(
            function() {
            }));
};


/**
 * @return {!Datum}
 */
r5js.TrampolineHelper.prototype.getInputPort = function() {
    return this.inputPort_;
};


/**
 * @return {!Datum}
 */
r5js.TrampolineHelper.prototype.getOutputPort = function() {
    return this.outputPort_;
};