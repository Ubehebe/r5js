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

goog.provide('r5js.Repl');


goog.require('r5js.valutil');



/**
 * @param {!r5js.Terminal} terminal
 * @param {!r5js.Evaluator} evaluator
 * @param {function(string): !goog.Promise<boolean>} isLineComplete
 * @struct
 * @constructor
 */
r5js.Repl = class {
    /**
     * @param {!r5js.Terminal} terminal
     * @param {!r5js.Evaluator} evaluator
     * @param {function(string): !goog.Promise<boolean>} isLineComplete
     */
    constructor(terminal, evaluator, isLineComplete) {
        /** @const @private */ this.terminal_ = terminal;
        /** @const @private */ this.evaluator_ = evaluator;
        /** @const @private */ this.isLineComplete_ = isLineComplete;
        /** @private */ this.awaitingEval_ = '';
    }

    /** Starts the read-eval-print loop. */
    start() {
        this.terminal_.getNextLineOfInput().then(
            this.handleInputLine, undefined /* opt_onRejected */, this);
    }

    /** @param {string} inputLine */
    handleInputLine(inputLine) {
        this.isLineComplete_(this.awaitingEval_ += inputLine + ' '
        ).then(function(complete) {
                if (complete) {
                    const toEval = this.awaitingEval_;
                    this.awaitingEval_ = '';
                    return this.evaluator_.evaluate(toEval);
                }
            }, undefined /* opt_onRejected */, this
        ).then(
            function(value) { this.terminal_.print(value); },
            function(error) {
                this.terminal_.error((/** @type {!r5js.Error} */ (error)).msg);
            }, this
        ).thenAlways(function() {
                this.terminal_.getNextLineOfInput().then(
                    this.handleInputLine, undefined /* opt_onRejected */, this);
            }, this);
    }
};