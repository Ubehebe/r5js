goog.provide('r5js.repl.jqconsole.main');


goog.require('r5js.InputPort');
goog.require('r5js.R5RSCompliantOutputPort');
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
        var stdout = new r5js.R5RSCompliantOutputPort(function(str) {
          jqconsole.Write(str, 'jqconsole-output');
        });
        var evaluator = r5js.boot(
            sources.syntax, sources.procedures, r5js.InputPort.NULL, stdout);
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
