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

goog.provide('r5js.platform.node.repl');


goog.require('goog.Promise');
goog.require('goog.array');
goog.require('r5js.CallbackBackedPort');
goog.require('r5js.InputPort');
goog.require('r5js.Repl');
goog.require('r5js.curPlatform');
goog.require('r5js.replutil');


/** The main REPL method. */
r5js.platform.node.repl = function() {
  var platform = /** @type {!r5js.platform.Node_} */ (
      r5js.curPlatform());
  /** @type {r5js.Terminal} */ var terminal;
  var stdin = r5js.InputPort.NULL;
  var stdout = new r5js.CallbackBackedPort(function(output) {
    terminal.print(output);
  });
  platform.newEvaluator(stdin, stdout).then(function(evaluator) {
    var isLineComplete = function(line) {
      return goog.Promise.resolve(r5js.replutil.isLineComplete(line));
    };
    terminal = platform.getTerminal();
    new r5js.Repl(terminal, evaluator, isLineComplete).start();
  });
};


goog.exportSymbol('r5js.repl.main', r5js.platform.node.repl);
// nodejs hack. See comment in goog.promise.testSuiteAdapter.
goog.exportSymbol('setTimeout', setTimeout);
