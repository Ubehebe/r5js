import {Error} from '../base/error';
import {Value} from "../base/value";
import {CdrHelper} from './cdr_helper';
import {CompoundDatum} from './compound_datum';
import {Datum} from './datum';
import {Pair} from './pair';
import {SiblingBuffer} from './sibling_buffer';

export class List extends Pair {

  private dirty = false;
  private dotted = false;

  constructor(firstChild: Datum | null) {
    super();
    firstChild && this.setFirstChild(firstChild);
  }

  markDirty() {
    this.dirty = true;
  }

  markDotted() {
    this.dotted = true;
  }

  /** @override */
  isImproperList() {
    return this.dotted;
  }

  /** @override */
  eqv(other: Datum): boolean {
    if (this.dotted) {
      return this === other;
    }

    if (!(other instanceof CompoundDatum)) {
      return false;
    }

    if (this === other
        || (other instanceof List
            && !this.getFirstChild()
            && !other.getFirstChild())) {
      return true;
    }

    const thisHelper = this.getCdrHelper();
    const otherHelper = other.getCdrHelper();
    if (thisHelper && otherHelper) {
      return thisHelper.equals(otherHelper);
    } else if (thisHelper && other instanceof CompoundDatum) {
      return thisHelper.resolvesTo(other);
    } else if (otherHelper) {
      return otherHelper.resolvesTo(this);
    } else {
      return false;
    }
  }

  car(): Value {
    return this.getFirstChild()!;
  }

  cdr(): Value {
    const startOfCdr = this.getFirstChild()!.getNextSibling();
    if (!startOfCdr) {
      return new SiblingBuffer().toList(List);
    }

    if (startOfCdr.getNextSibling() || !this.dirty) {
      // TODO bl investigate why this is happening
      if (startOfCdr.getNextSibling() === startOfCdr) {
        startOfCdr.setNextSibling(null);
      }
      const buf = new SiblingBuffer().appendSibling(startOfCdr);
      return this.dotted
          ? buf.toList(DottedList)
          : buf.toList(List);
    } else {
      return startOfCdr;
    }
  }
}

export class DottedList extends Pair {
  constructor(firstChild: Datum | null) {
    super();
    firstChild && this.setFirstChild(firstChild);
  }

  /** @override */
  isImproperList() {
    return true;
  }

  car(): Value {
    return this.getFirstChild()!;
  }

  cdr(): Value {
    const startOfCdr = this.getFirstChild()!.getNextSibling();
    let ans;
    if (startOfCdr) {
      if (startOfCdr.getNextSibling()) {
        ans = new SiblingBuffer()
            .appendSibling(startOfCdr)
            .toList(DottedList);
      } else {
        ans = startOfCdr;
      }
      if (ans instanceof CompoundDatum) {
        ans.setCdrHelper(new CdrHelperImpl(this, startOfCdr));
      }
      return ans;
    } else {
      return new SiblingBuffer().toList(List);
    }
  }
}

export class CdrHelperImpl implements CdrHelper {

  constructor(
      private readonly head: CompoundDatum,
      private readonly startOfCdr: Datum) {}

  /** @override */
  setCar(car: Datum) {
    if (this.head.isImmutable()) {
      throw Error.immutable(this.head.toString());
    }
    this.head.getFirstChild()!.setNextSibling(car);
  }

  /** @override */
  setCdr(cdr: Datum) {
    if (this.head.isImmutable()) {
      throw Error.immutable(this.head.toString());
    }
    this.startOfCdr.setNextSibling(cdr);
    if (!(cdr instanceof List)) {
      let cur: CdrHelperImpl | null = this;
      do {
        if (cur.head instanceof List) {
          cur.head.markDotted();
        }
      } while (cur = cur.head.getCdrHelper() as CdrHelperImpl);
    }
  }

  /** @override */
  equals(cdrHelper: CdrHelper): boolean {
    const asImpl = cdrHelper as CdrHelperImpl;
    return this.head === asImpl.head && this.startOfCdr === asImpl.startOfCdr;
  }

  /** @override */
  resolvesTo(datum: Datum) {
    if (!datum) {
      return false;
    } else if (this.head === datum) {
      return this.startOfCdr === (datum as CompoundDatum).getFirstChild();
    } else {
      return false;
    }
  }
}
