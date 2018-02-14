goog.module('r5js.InMemoryOutputPort');

const InMemoryPortBuffer = goog.require('r5js.InMemoryPortBuffer');
const OutputSavingPort = goog.require('r5js.OutputSavingPort');
const {addOutputPortImpl} = goog.require('r5js.OutputPort');

/** @struct @implements {OutputSavingPort} */
class InMemoryOutputPort {
  /** @param {!InMemoryPortBuffer} buffer */
  constructor(buffer) {
  /** @const @private */ this.buffer_ = buffer;
  /** @const @private {!Array<string>} */ this.outputs_ = [];
  }

  /** @override */
  write(str) {
    this.buffer_.append(str);
    this.outputs_.push(str);
  }

  /** @override */
  dequeueOutput() {
    return this.outputs_.shift();
  }

  /** @override */
  close() {}
}

addOutputPortImpl(InMemoryOutputPort);

exports = InMemoryOutputPort;