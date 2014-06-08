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

goog.provide('r5js.repl.main');


goog.require('goog.array');
goog.require('r5js.InputPort');
goog.require('r5js.Platform');
goog.require('r5js.R5RSCompliantOutputPort');
goog.require('r5js.Repl');
goog.require('r5js.boot');
goog.require('r5js.test.SchemeSources');


/** The main REPL method. */
r5js.repl.main = function() {
  var platform = r5js.Platform.get.apply(
      null, goog.array.toArray(arguments));
  r5js.test.SchemeSources.get(platform.fetchUrl.bind(platform)).
      then(function(sources) {
        var evaluator = r5js.boot(sources.syntax, sources.procedures);
        var terminal = platform.getTerminal(
            evaluator.willParse.bind(evaluator));
        var stdin = r5js.InputPort.NULL;
        var stdout = new r5js.R5RSCompliantOutputPort(
            terminal.print.bind(terminal));
        new r5js.Repl(terminal, evaluator.withPorts(stdin, stdout)).start();
      });
};


goog.exportSymbol('r5js.repl.main', r5js.repl.main);
// nodejs hack. See comment in goog.promise.testSuiteAdapter.
goog.exportSymbol('setTimeout', setTimeout);
