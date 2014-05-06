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
 * @param {!r5js.IPipeline} pipeline A pipeline object.
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


/**
 * Mainly intended for multiline input on a terminal:
 * when the programmer presses enter, the terminal needs to know
 * whether to send the line out for evaluation or wait for another
 * line to complete the input.
 * @param {string} logicalLine The logical line.
 * @return {boolean} True iff the logical line has a parse.
 */
r5js.EvaluatorImpl.prototype.willParse = function(logicalLine) {
  try {
    this.pipeline_.parse(/** @type {!r5js.Datum} */ (
        this.pipeline_.read(
        this.pipeline_.scan(logicalLine))));
    return true;
  } catch (x) {
    /* If parsing failed, we usually want to wait for another line
         of input. There's one common exception: unquoted empty lists
         () and nested versions of the same. If a programmer types ()
         at the terminal and presses enter, she will be stuck forever:
         nothing she later types in will make the line buffer parse, and
         so the terminal will never send the line buffer off for
         evaluation. As a heuristic, if the parse has not succeeded,
         we return false unless the number of opening and closing parens
         is the same. This might not be the right heuristic,
         but I haven't found a counterexample yet. Note that it's
         fine to type unquoted empty lists as their own lines as long
         as they are not the first line: for example the following is
         fine:

         >> (define-syntax
         >> foo
         >> (syntax-rules
         >> ()
         >> ((foo f) 'hi)))
         >> (foo ())

         If we find more of these situations where parsing fails but
         we should not wait for more input, it might be a better idea
         to equip the programmer with a button or key to flush the
         line buffer. */
    var lparens = logicalLine.match(/\(/g);
    var rparens = logicalLine.match(/\)/g);
    return !!(lparens && rparens && lparens.length === rparens.length);
  }
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
  return r5js.ToStringEvaluator.schemeValueToDisplayString(
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
