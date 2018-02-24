import {CompoundDatum} from './compound_datum';
import {Datum} from './datum';

export class UnquoteSplicing extends CompoundDatum {
  constructor(firstChild: Datum) {
    super();
    firstChild && this.setFirstChild(firstChild);
  }

  /** @override */
  setQuasiquotationLevel(qqLevel: number): CompoundDatum {
    this.qqLevel = qqLevel;
    return super.setQuasiquotationLevel(qqLevel - 1);
  }
}
