import {Datum} from './datum';

/**
 * See the comment to {@link r5js.Datum.siblingsToList}
 * for an explanation of what this interface does.
 * TODO: this should be an interface, not a class
 */
export class CdrHelper {

  /** Basically, call {@code set-car!} on the master list. */
  setCar(car: Datum) {
  }

  /** Basically, call {@code set-cdr!} on the master list. */
  setCdr(cdr: Datum) {
  }

  /** @return True iff the two CdrHelpers point to the same list and have the same offset. */
  equals(cdrHelper: CdrHelper): boolean {
    return false;
  }

  /**
   * @return True iff the CdrHelper points to the given list datum
   * and its offset is that list's first child.
   */
  resolvesTo(datum: Datum): boolean {
    return false;
  }
}