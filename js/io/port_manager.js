goog.module('r5js.PortManager');

const InMemoryInputPort = goog.require('r5js.InMemoryInputPort');
const {InMemoryOutputPort} = require('/js/io/in_memory_output_port_collect_es6_sources.es6/node_modules/__main__/js/io/in_memory_output_port');
const InputPort = goog.require('r5js.InputPort');
const {InMemoryPortBuffer} = require('/js/io/in_memory_port_buffer_collect_es6_sources.es6/node_modules/__main__/js/io/in_memory_port_buffer');
const {OutputPort} = require('/js/io/output_port_collect_es6_sources.es6/node_modules/__main__/js/io/output_port');

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
     * @suppress {checkTypes} Closure Compiler doesn't understand that InMemoryOutputPort is an OutputPort
     */
    newOutputPort(name) {
        if (!(name in this.buffers_)) {
            this.buffers_[name] = new InMemoryPortBuffer();
        }
        return new InMemoryOutputPort(this.buffers_[name]);
    }
}

exports = PortManager;
