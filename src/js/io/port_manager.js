goog.module('r5js.PortManager');

const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const InMemoryInputPort = goog.require('r5js.InMemoryInputPort');
const InMemoryOutputPort = goog.require('r5js.InMemoryOutputPort');
const InMemoryPortBuffer = goog.require('r5js.InMemoryPortBuffer');

class PortManager {
    constructor() {
        /** @const @private {!Object<string, !InMemoryPortBuffer>} */
        this.buffers_ = {};
    }

    /**
     * @param {string} name
     * @return {!InputPort}
     */
    newInputPort(name) {
        if (!(name in this.buffers_)) {
            this.buffers_[name] = new InMemoryPortBuffer();
        }
        return new InMemoryInputPort(this.buffers_[name]);
    }

    /**
     * @param {string} name
     * @return {!OutputPort}
     */
    newOutputPort(name) {
        if (!(name in this.buffers_)) {
            this.buffers_[name] = new InMemoryPortBuffer();
        }
        return new InMemoryOutputPort(this.buffers_[name]);
    }
}

exports = PortManager;
