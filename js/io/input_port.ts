import {Character} from '../ast/character';

export class /* TODO should be interface */
InputPort {
  isCharReady(): boolean {
    return false;
  }

  /** @return The next character, or null if there are no more characters. */
  peekChar(): Character | null {
    return null;
  }

  /** @return The next value, or null if there are no more values. */
  read(): Value | null {
    return null;
  }

  /** @return The next character, or null if there are no more characters. */
  readChar(): Character | null {
    return null;
  }

  /** @see R5RS 6.6.1 */
  close() {
  }
}

const IMPLEMENTED_BY_PROP_ = '$r5js.InputPort';

export function isInputPortImpl(obj: any): boolean {
  return !!(obj && obj[IMPLEMENTED_BY_PROP_]);
}

export function addInputPortImpl(ctor: any) {
  ctor.prototype[IMPLEMENTED_BY_PROP_] = true;
}

/** An input port that has no available input. */
class NullInputPort extends InputPort {
  constructor() {
    super();
  }
}

addInputPortImpl(NullInputPort);

export const NULL_INPUT_PORT: InputPort = new NullInputPort();