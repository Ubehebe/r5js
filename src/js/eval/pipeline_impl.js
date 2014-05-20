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



goog.provide('r5js.PipelineImpl');


goog.require('r5js.Environment');
goog.require('r5js.InputPort');
goog.require('r5js.ParseError');
goog.require('r5js.ParserImpl');
goog.require('r5js.Reader');
goog.require('r5js.Scanner');
goog.require('r5js.trampoline');



/**
 * @param {!r5js.IEnvironment} rootEnv The root environment.
 * @implements {r5js.IPipeline}
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
  return new r5js.Reader(scanner).read();
};


/**
 * @param {!r5js.Datum} root
 * @param {!r5js.parse.Nonterminal=} opt_nonterminal
 * @return {!r5js.Datum}
 * TODO bl: why does the compiler not accept an. @override here?
 */
r5js.PipelineImpl.prototype.parse = function(root, opt_nonterminal) {
  var parser = new r5js.ParserImpl(root);
  var ans = goog.isDef(opt_nonterminal) ?
      parser.parse(opt_nonterminal) :
      parser.parse();
  if (!ans) {
    throw new r5js.ParseError(root);
  }
  return ans;
};


/** @override */
r5js.PipelineImpl.prototype.desugar = function(root) {
  return root.desugar(this.env_, false);
};


/** @override */
r5js.PipelineImpl.prototype.desugarRepl = function(root) {
  return root.desugar(this.env_, false);
};


/** @override */
r5js.PipelineImpl.prototype.Eval = function(
    continuable, inputPort, outputPort) {
  return r5js.trampoline(continuable, this.env_, inputPort, outputPort);
};
