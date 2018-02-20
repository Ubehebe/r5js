import {Datum} from './datum';
import {SimpleDatum} from './simple_datum';

/**
 * TODO bl this class should not exist. It's used only as a shim in
 * {@link r5js.Environment#get}.
 */
export class Ref extends SimpleDatum<Datum> {
  /** @param {!Datum} deref Datum to dereference. */
  constructor(deref: Datum) {
    super(deref);
  }

  deref(): Datum {
    return this.payload;
  }
}
