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



goog.provide('r5js.Pipeline');


goog.require('r5js.CallbackBackedPort');
goog.require('r5js.Environment');
goog.require('r5js.InputPort');
goog.require('r5js.ParseError');
goog.require('r5js.Parser');
goog.require('r5js.Reader');
goog.require('r5js.Scanner');
goog.require('r5js.trampoline');



/**
 * @implements {r5js.IPipeline}
 * @constructor
 */
r5js.Pipeline = function() {};


/** @override */
r5js.Pipeline.prototype.setRootEnv = function(rootEnv) {
  this.rootEnv = rootEnv;
  this.env = new r5js.Environment(rootEnv);
};


/** @override */
r5js.Pipeline.prototype.scan = function(string) {
  return new r5js.Scanner(string);
};


/** @override */
r5js.Pipeline.prototype.read = function(scanner) {
  return new r5js.Reader(scanner).read();
};


/**
 * @param {!r5js.Datum} root
 * @param {!r5js.parse.Nonterminal=} opt_nonterminal
 * @return {!r5js.Datum}
 * TODO bl: why does the compiler not accept an @override here?
 */
r5js.Pipeline.prototype.parse = function(root, opt_nonterminal) {
  var parser = new r5js.Parser(root);
  var ans = goog.isDef(opt_nonterminal) ?
      parser.parse(opt_nonterminal) :
      parser.parse();
  if (!ans) {
      throw new r5js.ParseError(root);
  }
  return ans;
};


/** @override */
r5js.Pipeline.prototype.desugar = function(root, replMode) {
  if (!replMode) {
    this.env = new r5js.Environment(this.rootEnv);
  }
  return root.desugar(this.env, false).setStartingEnv(this.env);
};


/** @override */
r5js.Pipeline.prototype.Eval = function(continuable, onOutput) {
  return r5js.trampoline(
      continuable,
      r5js.InputPort.NULL,
      new r5js.CallbackBackedPort(onOutput));
};
