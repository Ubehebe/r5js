import {CdrHelper} from './cdr_helper';
import {CompoundDatum} from './compound_datum';
import {Datum} from './datum';
import {SiblingBuffer} from './sibling_buffer';
import {addImplementation} from './ipair';
import {Error} from '../error';
import {Value} from "../value";

export class List extends CompoundDatum /* implicitly implements IPair (structural interface) */ {

  private dirty: boolean = false;
  private dotted: boolean = false;

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
  eqv(other) {
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
    let otherHelper = other.getCdrHelper();
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

addImplementation(List);

export class DottedList extends CompoundDatum /* implicitly implements IPair (structural interface) */ {
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

addImplementation(DottedList);

export class CdrHelperImpl extends CdrHelper {

  constructor(private head: CompoundDatum, private startOfCdr: Datum) {
    super();
  }

  /** @override */
  setCar(car) {
    if (this.head.isImmutable()) {
      throw Error.immutable(this.head.toString());
    }
    this.head.getFirstChild()!.setNextSibling(car);
  }

  /** @override */
  setCdr(cdr) {
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
  equals(cdrHelper) {
    const asImpl = cdrHelper;
    return this.head === asImpl.head && this.startOfCdr === asImpl.startOfCdr;
  }

  /** @override */
  resolvesTo(datum) {
    if (!datum) {
      return false;
    } else if (this.head === datum) {
      return this.startOfCdr === datum.getFirstChild();
    } else {
      return false;
    }
  }
}