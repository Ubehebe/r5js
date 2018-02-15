import {InMemoryPortBuffer} from './in_memory_port_buffer';
import {OutputSavingPort} from './output_saving_port';
import {addOutputPortImpl} from './output_port';

export class InMemoryOutputPort extends OutputSavingPort {

  private readonly outputs: string[] = [];

  constructor(private readonly buffer: InMemoryPortBuffer) {
    super();
  }

  /** @override */
  write(str) {
    this.buffer.append(str);
    this.outputs.push(str);
  }

  /** @override */
  dequeueOutput() {
    return this.outputs.shift() || null;
  }

  /** @override */
  close() {
  }
}

addOutputPortImpl(InMemoryOutputPort);