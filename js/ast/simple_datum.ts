import {Datum} from "./datum";

export class SimpleDatum<T> extends Datum {

  protected payload: T;

  constructor(payload: T) {
    super();
    this.payload = payload;
  }

  /**
   * Booleans, characters, and numbers have value equality semantics.
   * @override
   */
  eqv(other: Datum): boolean {
    return other instanceof SimpleDatum
        && this.payload === other.payload;
  }

  getPayload(): T {
    return this.payload;
  }

  setPayload(payload: T) {
    this.payload = payload;
  }

  /** @override */
  clone(parent: Datum | undefined = undefined): Datum {
    const clone = super.clone(parent) as SimpleDatum<T>;
    clone.setPayload(this.payload);
    return clone;
  }

  /**
   * TODO bl: this is intended to have the exact semantics of the library
   * procedure equal?, but I'm not sure that it does.
   */
  isEqual(other: Datum): boolean {
    return other instanceof SimpleDatum
        && this.payload === other.payload;
  }

  /**
   * Datums representing identifiers, strings, and characters all have payloads of type string.
   * If they all unwrapped as JavaScript strings, it would be impossible to re-wrap them correctly
   * (noninjective mapping). We choose to store identifiers unwrapped because they're expected to
   * be more common than the other two.
   *
   * Environment specifiers cannot be unwrapped to their Environment payloads because Environment
   * values in Environments already have a meaning, namely, a redirect to look up the name in some
   * other Environment.
   *
   * Finally, the vector stuff may need to be overhauled.
   */
  unwrap() {
    return this.payload;
  }
}