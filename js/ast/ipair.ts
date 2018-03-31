/**
 * TODO: interfaces don't exist at runtime. The Closure idiom for runtime
 * instanceof detection is below. TypeScript has a separate mechanism, called instanceof
 * type guards. We can't use that until all the callers of isImplementedBy are migrated to
 * TypeScript.
 */
import {Value} from "../value";

export interface IPair {
  car(): Value;
  cdr(): Value;
}

const IMPLEMENTED_BY_PROP = '$r5js.IPair';

export function isImplementedBy(obj: any): boolean {
  return !!(obj && obj[IMPLEMENTED_BY_PROP]);
}

export function addImplementation(ctor: any) {
  ctor.prototype[IMPLEMENTED_BY_PROP] = true;
}