import {Value} from "../value";
import {CompoundDatum} from "./compound_datum";

/**
 * This is an abstract class instead of an interface to allow runtime instanceof checks from the
 * `pair?` primitive procedure.
 */
export abstract class Pair extends CompoundDatum {
  constructor() {
    super();
  }

  abstract car(): Value;
  abstract cdr(): Value;
}