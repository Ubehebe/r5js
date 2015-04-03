goog.provide('r5js.platform.node.Terminal');

goog.require('goog.Promise');
goog.require('r5js.Terminal');

r5js.platform.node.Terminal = /** @implements {r5js.Terminal} */ class {
  constructor() {
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
  }

  /** @override */
  getNextLineOfInput() {
    return new goog.Promise(function (resolve) {
      this.readline_.once('line', resolve);
    }, this);
  }

  /** @override */
  print(str) {
    console.log(str);
    this.readline_.prompt(); // TODO bl double-prompts on Scheme output
  }

  /** @override */
  error(str) {
    console.error(str);
    this.readline_.prompt(); // TODO bl double-prompts on Scheme output
  }

  /** @private */
  handleClose_() {
    process.exit(0);
  }
};


/** @const @private */ r5js.platform.node.Terminal.PROMPT_ = '>> ';
