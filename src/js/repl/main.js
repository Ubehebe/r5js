goog.provide('r5js.repl.main');

goog.require('goog.array');
goog.require('r5js.InputPort');
goog.require('r5js.R5RSCompliantOutputPort');
goog.require('r5js.Repl');
goog.require('r5js.boot');
goog.require('r5js.js.Environment');
goog.require('r5js.test.SchemeSources');


/** The main REPL method. */
r5js.repl.main = function() {
  var jsEnv = r5js.js.Environment.get.apply(
      null, goog.array.toArray(arguments));
  r5js.test.SchemeSources.get(jsEnv.fetchUrl.bind(jsEnv)).
      then(function(sources) {
        var terminal = jsEnv.getTerminal();
        var stdin = r5js.InputPort.NULL;
        var stdout = new r5js.R5RSCompliantOutputPort(
            terminal.print.bind(terminal));
        var evaluator = r5js.boot(
            sources.syntax, sources.procedures, stdin, stdout);
        new r5js.Repl(terminal, evaluator).start();
      });
};


goog.exportSymbol('r5js.repl.main', r5js.repl.main);
// nodejs hack. See comment in goog.promise.testSuiteAdapter.
goog.exportSymbol('setTimeout', setTimeout);
