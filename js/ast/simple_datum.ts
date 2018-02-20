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
  eqv(other) {
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
  clone(parent: Datum | null): Datum {
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

  /** @override */
  unwrap() {
    return this.payload;
  }
}