goog.provide('r5js.platform.node.Terminal');


goog.require('goog.Promise');
goog.require('r5js.Terminal');



/**
 * @implements {r5js.Terminal}
 * @struct
 * @constructor
 */
r5js.platform.node.Terminal = function() {
  var readline = require('readline');
  /** @private @const */
  this.readline_ = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: true,
    completer: goog.nullFunction
  });
  this.readline_.setPrompt(
      r5js.platform.node.Terminal.PROMPT_,
      r5js.platform.node.Terminal.PROMPT_.length);
  this.readline_.on('close', this.handleClose_.bind(this));
  this.readline_.prompt();
};


/** @override */
r5js.platform.node.Terminal.prototype.getNextLineOfInput = function() {
  return new goog.Promise(function(resolve) {
    this.readline_.once('line', resolve);
  }, this);
};


/** @override */
r5js.platform.node.Terminal.prototype.print = function(str) {
  console.log(str);
  this.readline_.prompt(); // TODO bl double-prompts on Scheme output
};


/** @override */
r5js.platform.node.Terminal.prototype.error = function(str) {
  console.error(str);
  this.readline_.prompt(); // TODO bl double-prompts on Scheme output
};


/** @private */
r5js.platform.node.Terminal.prototype.handleClose_ = function() {
  process.exit(0);
};


/** @const @private */ r5js.platform.node.Terminal.PROMPT_ = '>> ';
