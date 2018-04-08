import {Datum} from './datum';

export interface CdrHelper {

  /** Basically, call {@code set-car!} on the master list. */
  setCar(car: Datum): void;

  /** Basically, call {@code set-cdr!} on the master list. */
  setCdr(cdr: Datum): void;

  /** @return True iff the two CdrHelpers point to the same list and have the same offset. */
  equals(cdrHelper: CdrHelper): boolean;

  /**
   * @return True iff the CdrHelper points to the given list datum and its offset is that list's
   * first child.
   */
  resolvesTo(datum: Datum): boolean;
}