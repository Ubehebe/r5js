goog.provide('r5js.repl.jqconsole.main');


goog.require('r5js.EvalAdapter');
goog.require('r5js.InMemoryInputPort');
goog.require('r5js.InMemoryOutputPort');
goog.require('r5js.boot');
goog.require('r5js.js.Environment');
goog.require('r5js.test.SchemeSources');


/**
 * @param {?} jqconsole
 * @suppress {checkTypes} for the jq-console integration
 */
r5js.repl.jqconsole.main = function(jqconsole) {
  var jsEnv = r5js.js.Environment.get();
  r5js.test.SchemeSources.get(jsEnv.fetchUrl.bind(jsEnv)).
      then(function(sources) {
        var buffer = [];
        var stdin = new r5js.InMemoryInputPort(buffer);
        var stdout = new r5js.InMemoryOutputPort(buffer);
        var evaluator = r5js.boot(
            sources.syntax, sources.procedures, stdin, stdout);
        var ugh = function() {
          jqconsole.Prompt(true, function(input) {
            jqconsole.Write(
                r5js.EvalAdapter.toDisplayString(
                            evaluator.evaluate(input)) + '\n',
                'jqconsole-output');
            ugh();
          });
        };
        ugh();
      });
};
