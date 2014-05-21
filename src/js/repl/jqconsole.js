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
        new r5js.repl.jqconsole.Console_(evaluator, jqconsole).prompt();
      });
};



/**
 * @param {!r5js.Evaluator} evaluator
 * @param {?} jqconsole
 * @struct
 * @constructor
 * @private
 */
r5js.repl.jqconsole.Console_ = function(evaluator, jqconsole) {
  /** @const @private */ this.evaluator_ = evaluator;
  /** @const @private */ this.jqconsole_ = jqconsole;
};


/** @suppress {checkTypes} for the jqconsole integration */
r5js.repl.jqconsole.Console_.prototype.prompt = function() {
  this.jqconsole_.Prompt(
      true /* history_enabled */, this.handleInput_.bind(this));
};


/**
 * @param {string} input
 * @private
 * @suppress {checkTypes} for the jqconsole integration
 */
r5js.repl.jqconsole.Console_.prototype.handleInput_ = function(input) {
  var value;
  var outputClass = 'jqconsole-output';
  try {
    value = r5js.EvalAdapter.toDisplayString(
        this.evaluator_.evaluate(input));
  } catch (e) {
    value = e.toString();
    outputClass = 'jqconsole-error';
  } finally {
    this.jqconsole_.Write(value + '\n', outputClass);
    this.prompt();
  }
};
