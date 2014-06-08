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


goog.require('r5js.EvalAdapter');



/**
 * @param {!r5js.Terminal} terminal
 * @param {!r5js.Evaluator} evaluator
 * @struct
 * @constructor
 */
r5js.Repl = function(terminal, evaluator) {
  /** @const @private */ this.terminal_ = terminal;
  /** @const @private */ this.evaluator_ = evaluator;
  /** @private */ this.awaitingEval_ = '';
};


/** Starts the read-eval-print loop. */
r5js.Repl.prototype.start = function() {
  this.terminal_.getNextLineOfInput().then(
      this.handleInputLine, undefined /* opt_onRejected */, this);
};


/** @param {string} inputLine */
r5js.Repl.prototype.handleInputLine = function(inputLine) {
  this.evaluator_.willParse(this.awaitingEval_ += inputLine + ' '
  ).then(function(ok) {
    if (ok) {
      var toEval = this.awaitingEval_;
      this.awaitingEval_ = '';
      return this.evaluator_.evaluate(toEval);
    }
  }, undefined /* opt_onRejected */, this
  ).then(r5js.EvalAdapter.toDisplayString
  ).then(
      function(displayString) { this.terminal_.print(displayString); },
      function(error) { this.terminal_.error(error.toString()); },
      this
  ).thenAlways(function() {
    this.terminal_.getNextLineOfInput().then(
        this.handleInputLine, undefined /* opt_onRejected */, this);
  }, this);
};
