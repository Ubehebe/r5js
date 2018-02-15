import {OutputPort, addOutputPortImpl} from './output_port';

export class CallbackBackedPort extends OutputPort {
  /** @param onOutput Callback that will be called whenever output is available. */
  constructor(private readonly onOutput: ((s: string) => void)) {
    super();
  }

  /** @override */
  close() {
  }

  /**
   * @suppress {reportUnknownTypes}
   * @override
   */
  write(str) {
    this.onOutput(str);
  }
}

addOutputPortImpl(CallbackBackedPort);