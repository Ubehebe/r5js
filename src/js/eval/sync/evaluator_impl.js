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


goog.provide('r5js.sync.EvaluatorImpl');


goog.require('r5js.sync.Evaluator');
goog.require('r5js.valutil');



/**
 * @param {!r5js.Pipeline} pipeline
 * @param {!r5js.InputPort} inputPort
 * @param {!r5js.OutputPort} outputPort
 * @implements {r5js.sync.Evaluator}
 * @struct
 * @constructor
 */
r5js.sync.EvaluatorImpl = function(pipeline, inputPort, outputPort) {
  /** @const @private */ this.pipeline_ = pipeline;
  /** @const @private */ this.inputPort_ = inputPort;
  /** @const @private */ this.outputPort_ = outputPort;
};


/** @override */
r5js.sync.EvaluatorImpl.prototype.evaluate = function(input) {
  return r5js.valutil.toWriteString(
      this.pipeline_.Eval(
      this.pipeline_.desugar(
      this.pipeline_.parse(/** @type {!r5js.Datum} */ (
      this.pipeline_.read(
      this.pipeline_.scan(input))))),
      this.inputPort_,
      this.outputPort_));
};
