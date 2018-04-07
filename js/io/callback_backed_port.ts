import {OutputPort} from './output_port';

export class CallbackBackedPort extends OutputPort {
  /** @param onOutput Callback that will be called whenever output is available. */
  constructor(private readonly onOutput: ((s: string) => void)) {
    super();
  }

  close() {
  }

  write(str: string) {
    this.onOutput(str);
  }
}