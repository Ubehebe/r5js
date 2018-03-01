import {InMemoryPortBuffer} from "./in_memory_port_buffer";
import {InputPort} from "./input_port";
import {InMemoryInputPort} from "./in_memory_input_port";
import {OutputPort} from "./output_port";
import {InMemoryOutputPort} from "./in_memory_output_port";

export class PortManager {

  private readonly buffers_: { [key: string]: InMemoryPortBuffer } = {};

  constructor() {
  }

  newInputPort(name: string): InputPort {
    if (!(name in this.buffers_)) {
      this.buffers_[name] = new InMemoryPortBuffer();
    }
    return new InMemoryInputPort(this.buffers_[name]);
  }

  newOutputPort(name: string): OutputPort {
    if (!(name in this.buffers_)) {
      this.buffers_[name] = new InMemoryPortBuffer();
    }
    return new InMemoryOutputPort(this.buffers_[name]);
  }
}
