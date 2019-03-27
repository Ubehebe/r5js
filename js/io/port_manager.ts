import {InMemoryOutputPort} from "./in_memory_output_port";
import {InMemoryPortBuffer} from "./in_memory_port_buffer";
import {InputPort} from "./input_port";
import {OutputPort} from "./output_port";
import {InMemoryInputPort} from "./in_memory_input_port";

export class PortManager {

  private readonly buffers: { [key: string]: InMemoryPortBuffer } = {};

  newInputPort(name: string): InputPort {
    if (!(name in this.buffers)) {
      this.buffers[name] = new InMemoryPortBuffer();
    }
    return new InMemoryInputPort(this.buffers[name]);
  }

  newOutputPort(name: string): OutputPort {
    if (!(name in this.buffers)) {
      this.buffers[name] = new InMemoryPortBuffer();
    }
    return new InMemoryOutputPort(this.buffers[name]);
  }
}
