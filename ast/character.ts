import {SimpleDatum} from './simple_datum';

export class Character extends SimpleDatum<string> {

  constructor(c: string) {
    super(c);
  }

  /**
   * Datums representing characters have payloads of type string.
   * If they unwrapped as JavaScript strings, it would be impossible
   * to re-wrap them correctly (noninjective mapping). We choose to store
   * identifiers unwrapped because they're expected to be more common than
   * the other two.
   *
   * @override
   */
  unwrap(): any {
    return this;
  }
}
