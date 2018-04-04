import {CompoundDatum} from './compound_datum';
import {Datum} from './datum';

/**
 * TODO bl: this class illustrates the interpreter's confusion of abstract
 * syntax, intermediate representation, and runtime representation.
 * Having Scheme vectors be backed by JavaScript arrays is a reasonable
 * runtime decision, but is independent of AST and IR decisions.
 * This constructor cannot call {@link #convertVectorToArrayBacked}
 * on its argument; that changes the structure of the syntax tree,
 * which affects parsing. Only the runtime primitive procedures can do this.
 */
export class Vector extends CompoundDatum {

  private isArrayBacked: boolean;
  private readonly array: Datum[];

  constructor(firstChildOrArray: Datum | Datum[]) {
    super();

    this.isArrayBacked = firstChildOrArray instanceof Array;

    if (this.isArrayBacked) {
      this.array = firstChildOrArray as Datum[];
    } else {
      this.array = [];
      this.setFirstChild(firstChildOrArray as Datum);
    }
  }

  vectorLength(): number {
    if (!this.isArrayBacked) {
      this.convertVectorToArrayBacked();
    }
    return this.array.length;
  }

  vectorRef(index: number): Datum {
    if (!this.isArrayBacked) {
      this.convertVectorToArrayBacked();
    }
    return this.array[index];
  }

  vectorSet(index: number, val: Datum) {
    if (!this.isArrayBacked) {
      this.convertVectorToArrayBacked();
    }
    this.array[index] = val;
  }

  /**
   * Vector literals are constructed by the reader as linked lists
   * with no random access, while vectors created programmatically
   * via make-vector can just use JavaScript arrays. Instead of building
   * logic into the reader to convert its inefficient vectors to array-backed
   * ones, we check in every primitive vector procedure if the vector
   * is array-backed, and mutate it in place if it isn't. There may
   * be bugs involving the lost child/sibling pointers.
   */
  private convertVectorToArrayBacked() {
    this.forEachChild(child => this.array.push(child));
    this.isArrayBacked = true;
  }
}
