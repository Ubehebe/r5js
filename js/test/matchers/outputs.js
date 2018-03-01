goog.module('output');

const Matcher = goog.require('tdd.matchers.Matcher');
const {OutputSavingPort} = require('/js/io/io_collect_es6_sources.es6/node_modules/__main__/js/io/output_saving_port');

/**
 * @param {string} output
 * @return {!Matcher}
 */
function output(output) {
  return new HasOutput(output, /** @type {!OutputSavingPort} */ (sharedOutputPort));
}

/** @implements {Matcher} */
class HasOutput {
  /**
   * @param {string} expectedOutput
   * @param {!OutputSavingPort} outputPort
   */
  constructor(expectedOutput, outputPort) {
  /** @const @private */ this.expectedOutput_ = expectedOutput;
  /** @private {?string} */ this.actualOutput_ = null;
  /** @const @private */ this.outputPort_ = outputPort;
  }

  /** @override */
  matches(input) {
    this.actualOutput_ = this.outputPort_.dequeueOutput();
    return this.actualOutput_ === this.expectedOutput_;
  }

  /** @override */
  getFailureMessage(input) {
    return 'want ' + this.expectedOutput_ + ' got ' + this.actualOutput_;
  }
}

/** @private {?OutputSavingPort} */
let sharedOutputPort = null;

/** @param {!OutputSavingPort} outputPort */
function setOutputPort(outputPort) {
  sharedOutputPort = outputPort;
}

exports = {output, setOutputPort};
