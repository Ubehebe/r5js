import {SimpleDatum} from './simple_datum';

export class String extends SimpleDatum<string> {

  constructor(s: string) {
    super(s);
  }

  /**
   * Unlike other simple datums, strings have reference equality semantics.
   * @see R5RS 6.1
   * @override
   */
  eqv(other): boolean {
    return this === other;
  }

  /**
   * Datums representing strings have payloads of type string.
   * If they all unwrapped as JavaScript strings, it would be impossible
   * to re-wrap them correctly (noninjective mapping). We choose to store
   * identifiers unwrapped because they're expected to be more common than
   * strings.
   *
   * @override
   */
  unwrap(): any {
    return this;
  }
}