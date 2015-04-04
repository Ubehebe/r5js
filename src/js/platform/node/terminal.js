goog.module('r5js.platform.node.Terminal');

const promise = goog.require('goog.Promise');
const terminal = goog.require('r5js.Terminal');

/** @implements {r5js.Terminal} */
class Terminal {
  constructor() {
    const readline = require('readline');
    /** @private @const */
    this.readline_ = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      terminal: true,
      completer: goog.nullFunction
    });
    this.readline_.setPrompt(
        Terminal.PROMPT_,
        Terminal.PROMPT_.length);
    this.readline_.on('close', this.handleClose_.bind(this));
    this.readline_.prompt();
  }

  /** @override */
  getNextLineOfInput() {
    return new promise(function(resolve) {
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
}


/** @const @private */ Terminal.PROMPT_ = '>> ';

exports = Terminal;
