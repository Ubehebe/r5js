/* Copyright 2011-2013 Brendan Linn

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


goog.provide('r5js.EvaluatorImpl');



/**
 * @param {!r5js.Pipeline} pipeline A pipeline object.
 * @param {!r5js.InputPort} inputPort Input port to connect the evaluator to.
 * @param {!r5js.OutputPort} outputPort Output port to connect the evaluator to.
 * @implements {r5js.Evaluator}
 * @struct
 * @constructor
 */
r5js.EvaluatorImpl = function(pipeline, inputPort, outputPort) {
  /** @const @private */ this.pipeline_ = pipeline;
  /** @const @private */ this.inputPort_ = inputPort;
  /** @const @private */ this.outputPort_ = outputPort;
};


/** @override */
r5js.EvaluatorImpl.prototype.evaluate = function(string) {
  return this.pipeline_.Eval(
      this.pipeline_.desugar(
      this.pipeline_.parse(/** @type {!r5js.Datum} */ (
      this.pipeline_.read(
      this.pipeline_.scan(string))))),
      this.inputPort_,
      this.outputPort_);
};


/**
 * Just like {@link r5js.EvaluatorImpl.eval}, but reuses the old environment.
 * @param {string} string The source text to evaluate.
 * @return {string} A string representation of the value of the evaluation.
 */
r5js.EvaluatorImpl.prototype.repl = function(string) {
  return r5js.EvalAdapter.toDisplayString(
      this.pipeline_.Eval(
      this.pipeline_.desugarRepl(
      this.pipeline_.parse(/** @type {!r5js.Datum} */ (
      this.pipeline_.read(
      this.pipeline_.scan(string))))),
      this.inputPort_,
      this.outputPort_));
};


/** @override */
r5js.EvaluatorImpl.prototype.withPorts = function(inputPort, outputPort) {
  return new r5js.EvaluatorImpl(this.pipeline_, inputPort, outputPort);
};
