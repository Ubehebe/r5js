import {InMemoryPortBuffer} from './in_memory_port_buffer';
import {OutputPort} from "./output_port";

/**
 * OutputPort that also saves calls to write to a buffer, where they can be
 * {@link #dequeueOutput retrieved}.
 */
export class InMemoryOutputPort extends OutputPort {

  private readonly outputs: string[] = [];

  constructor(private readonly buffer: InMemoryPortBuffer) {
    super();
  }

  write(str) {
    this.buffer.append(str);
    this.outputs.push(str);
  }

  dequeueOutput(): string|null {
    return this.outputs.shift() || null;
  }
}