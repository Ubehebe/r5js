goog.provide('r5js.repl.node');
goog.provide('r5js.repl.node.main');


goog.require('r5js.EvalAdapter');
goog.require('r5js.boot');
goog.require('r5js.js.Environment');
goog.require('r5js.test.SchemeSources');



/**
 * @param {!r5js.Evaluator} evaluator
 * @param {!readline.Interface} readline
 * @struct
 * @constructor
 * @private
 */
r5js.repl.Node_ = function(evaluator, readline) {
  /** @private @const */ this.evaluator_ = new r5js.EvalAdapter(
      evaluator, r5js.EvalAdapter.toWriteString);
  /** @private @const */ this.readline_ = readline;
  this.readline_.on('line', this.handleLine_.bind(this));
  this.readline_.on('close', this.handleClose_.bind(this));
};


/**
 * @param {string} line
 * @private
 */
r5js.repl.Node_.prototype.handleLine_ = function(line) {
  console.log(this.evaluator_.evaluate(line));
  this.readline_.prompt();
};


/** @private */
r5js.repl.Node_.prototype.handleClose_ = function() {
  process.exit(0);
};


/**
 * Main entry point for the NodeJS REPL.
 * @suppress {checkTypes} TODO bl my Node version is 0.6x, but the externs
 * I'm using are for 0.10x, which has an incompatible readline API.
 */
r5js.repl.node.main = function() {
  var readline = require('readline');
  var rl = readline.createInterface(process.stdin, process.stdout);
  rl.setPrompt(
      r5js.repl.node.PROMPT_, r5js.repl.node.PROMPT_.length);

  var jsEnv = r5js.js.Environment.get();
  r5js.test.SchemeSources.get(jsEnv.fetchUrl.bind(jsEnv)).
      then(function(sources) {
        return r5js.boot(sources.syntax, sources.procedures);
      }).then(function(evaluator) {
        new r5js.repl.Node_(evaluator, /** @type {!readline.Interface} */ (rl));
      });

  rl.prompt();
};


/** @const @private */ r5js.repl.node.PROMPT_ = '>> ';


goog.exportSymbol('r5js.repl.node.main', r5js.repl.node.main);
// nodejs hack. See comment in goog.promise.testSuiteAdapter.
goog.exportSymbol('setTimeout', setTimeout);
