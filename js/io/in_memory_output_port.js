goog.module('r5js.InMemoryOutputPort');

const InMemoryPortBuffer = goog.require('r5js.InMemoryPortBuffer');
const OutputSavingPort = goog.require('r5js.OutputSavingPort');
const {addOutputPortImpl} = require('/js/io/output_port_collect_es6_sources.es6/node_modules/__main__/js/io/output_port');

class InMemoryOutputPort extends OutputSavingPort {
  /** @param {!InMemoryPortBuffer} buffer */
  constructor(buffer) {
    super();
  /** @const @private */ this.buffer_ = buffer;
  /** @const @private {!Array<string>} */ this.outputs_ = [];
  }

  /**
   * @suppress {reportUnknownTypes}
   * @override
   */
  write(str) {
    this.buffer_.append(str);
    this.outputs_.push(str);
  }

  /**
   * @suppress {reportUnknownTypes}
   * @override
   */
  dequeueOutput() {
    return this.outputs_.shift();
  }

  /** @override */
  close() {}
}

addOutputPortImpl(InMemoryOutputPort);

exports = InMemoryOutputPort;