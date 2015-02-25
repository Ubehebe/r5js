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



goog.provide('r5js.PipelineImpl');


goog.require('r5js.Environment');
goog.require('r5js.ParserImpl');
goog.require('r5js.Pipeline');
goog.require('r5js.ReaderImpl');
goog.require('r5js.Scanner');
goog.require('r5js.VACUOUS_PROGRAM');
goog.require('r5js.error');
goog.require('r5js.runtime.UNSPECIFIED_VALUE');
goog.require('r5js.trampoline');



/**
 * @param {!r5js.IEnvironment} rootEnv The root environment.
 * @implements {r5js.Pipeline}
 * @struct
 * @constructor
 */
r5js.PipelineImpl = function(rootEnv) {
  /** @const @private {!r5js.IEnvironment} */
  this.env_ = new r5js.Environment(rootEnv);
};


/** @override */
r5js.PipelineImpl.prototype.scan = function(string) {
  return new r5js.Scanner(string);
};


/** @override */
r5js.PipelineImpl.prototype.read = function(scanner) {
  return new r5js.ReaderImpl(scanner).read();
};


/** @override */
r5js.PipelineImpl.prototype.parse = function(root, opt_nonterminal) {
  var parser = new r5js.ParserImpl(root);
  var ans = goog.isDef(opt_nonterminal) ?
      parser.parse(opt_nonterminal) :
      parser.parse();
  if (!ans) {
    throw r5js.error.parse(root);
  }
  return ans;
};


/** @override */
r5js.PipelineImpl.prototype.desugar = function(root) {
  return /** @type {!r5js.ProcCallLike} */ (root.desugar(this.env_, false));
};


/**
 * @override
 * @suppress {checkTypes} TODO bl the compiler believes the ternary
 * always evaluates to false.
 */
r5js.PipelineImpl.prototype.Eval = function(
    continuable, inputPort, outputPort) {
  // r5js.VACUOUS_PROGRAM isn't a ProcCallLike, but this is enough of
  // a special case that I don't care.
  return continuable === r5js.VACUOUS_PROGRAM ?
      r5js.runtime.UNSPECIFIED_VALUE :
      r5js.trampoline(continuable, this.env_, inputPort, outputPort);
};
