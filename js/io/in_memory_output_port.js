goog.module('r5js.InMemoryOutputPort');

const {InMemoryPortBuffer} = require('/js/io/in_memory_port_buffer_collect_es6_sources.es6/node_modules/__main__/js/io/in_memory_port_buffer');
const {OutputSavingPort} = require('/js/io/output_saving_port_collect_es6_sources.es6/node_modules/__main__/js/io/output_saving_port');
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