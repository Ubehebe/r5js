import {InMemoryPortBuffer} from './in_memory_port_buffer';
import {OutputSavingPort} from './output_saving_port';

export class InMemoryOutputPort extends OutputSavingPort {

  private readonly outputs: string[] = [];

  constructor(private readonly buffer: InMemoryPortBuffer) {
    super();
  }

  write(str) {
    this.buffer.append(str);
    this.outputs.push(str);
  }

  dequeueOutput() {
    return this.outputs.shift() || null;
  }
}