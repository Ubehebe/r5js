import {Character} from '../ast/character';

/**
 * This is not an interface because we need to do frequent runtime type detection.
 * I wrote a type guard that checked for all the InputPort methods, but it caused
 * the tests to take longer than the default Jasmine timeout (5 seconds).
 */
export class InputPort {
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

export function isInputPort(obj: any): obj is InputPort {
  return obj instanceof InputPort;
}

/** An input port that has no available input. */
export const NULL_INPUT_PORT: InputPort = new InputPort();