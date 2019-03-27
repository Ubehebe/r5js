import {DEFINITION, FORMALS, LAMBDA_EXPRESSION, Nonterminal, VARIABLE} from "../parse/nonterminals";
import {isParserSensitiveId} from "../parse/rename_util";
import {CdrHelper} from "./cdr_helper";
import {Datum} from "./datum";
import {Identifier} from "./identifier";
import {RenameHelper} from "./rename_helper";
import {SiblingBuffer} from "./sibling_buffer";
import {SimpleDatum} from "./simple_datum";

export class CompoundDatum extends Datum {

  private firstChild: Datum | null = null;
  private cdrHelper: CdrHelper | null = null;
  protected qqLevel: number | undefined = undefined;

  constructor() {
    super();
  }

  getFirstChild(): Datum | null {
    return this.firstChild;
  }

  setFirstChild(firstChild: Datum) {
    this.firstChild = firstChild;
  }

  getQQLevel(): number | undefined {
    return this.qqLevel;
  }

  /** @override */
  clone(parent?: Datum): CompoundDatum {
    const ans = super.clone(parent) as CompoundDatum;
    if (this.firstChild) {
      const buf = new SiblingBuffer();
      this.forEachChild(child => buf.appendSibling(child.clone(ans)));
      ans.firstChild = buf.toSiblings();
    }
    return ans;
  }

  /**
   * TODO bl: this is intended to have the exact semantics of the library
   * procedure equal?, but I'm not sure that it does.
   */
  isEqual(other: CompoundDatum): boolean {
    let thisChild, otherChild;
    for (thisChild = this.firstChild, otherChild = other.firstChild;
         thisChild && otherChild;
         thisChild = thisChild.getNextSibling(),
             otherChild = otherChild.getNextSibling()) {
      if (thisChild instanceof CompoundDatum
          && otherChild instanceof CompoundDatum
          && !thisChild.isEqual(otherChild)) {
        return false;
      }
    }

    return !(thisChild || otherChild);
  }

  /** @override */
  fixParserSensitiveIds(helper: RenameHelper) {
    if (this.hasParse(LAMBDA_EXPRESSION)) {
      this.fixParserSensitiveIdsLambda_(helper);
    } else if (this.hasParse(DEFINITION)) {
      this.fixParserSensitiveIdsDef_(helper);
    } else {
      for (let cur = this.firstChild; cur; cur = cur.getNextSibling()) {
        cur.fixParserSensitiveIds(helper);
      }
    }
    super.fixParserSensitiveIds(helper);
  }

  /** TODO bl: document what this method does. */
  private fixParserSensitiveIdsLambda_(helper: RenameHelper) {
    const formalRoot = this.at(FORMALS);
    const newHelper = new RenameHelper(helper);

    if (formalRoot instanceof Identifier) { // (lambda x ...)
      const id = formalRoot.getPayload();
      if (isParserSensitiveId(id)) {
        formalRoot.setPayload(newHelper.addRenameBinding(id));
      }
    } else { // (lambda (x y) ...) or (lambda (x . y) ...)
      (formalRoot as CompoundDatum).forEachChild(
          child => {
            const formal = child as SimpleDatum<string>;
            const id = formal.getPayload();
            if (isParserSensitiveId(id)) {
              formal.setPayload(newHelper.addRenameBinding(id));
            }
          });
    }

    formalRoot!.getNextSibling()!.fixParserSensitiveIds(newHelper);
  }

  /** TODO bl: document what this method does. */
  private fixParserSensitiveIdsDef_(helper: RenameHelper) {
    const maybeVar = this.at(VARIABLE) as Identifier | null;
    let id;

    if (maybeVar) { // (define foo +)
      id = maybeVar.getPayload();
      if (isParserSensitiveId(id)) {
        maybeVar.setPayload(helper.addRenameBinding(id));
      }
    } else { // (define (foo x y) (+ x y))
      const vars = this.firstChild!.getNextSibling() as CompoundDatum;
      const name = vars.firstChild as Identifier;
      const newHelper = new RenameHelper(helper);
      for (let cur = name.getNextSibling(); cur; cur = cur.getNextSibling()) {
        const curId = cur as Identifier;
        id = curId.getPayload();
        if (isParserSensitiveId(id)) {
          curId.setPayload(newHelper.addRenameBinding(id));
        }
      }
      vars.getNextSibling()!.fixParserSensitiveIds(newHelper);
      const namePayload = name.getPayload();
      if (isParserSensitiveId(namePayload)) {
        name.setPayload(helper.addRenameBinding(namePayload));
      }
    }
  }

  at(type: Nonterminal): Datum | null {
    for (let cur = this.firstChild; cur; cur = cur.getNextSibling()) {
      if (cur.peekParse() === type) {
        return cur;
      }
    }
    return null;
  }

  setCdrHelper(cdrHelper: CdrHelper): this {
    this.cdrHelper = cdrHelper;
    return this;
  }

  getCdrHelper(): CdrHelper | null {
    return this.cdrHelper;
  }

  /**
   * @return The first child of this datum that is itself a list, or null if no such datum exists.
   */
  firstSublist(): CompoundDatum | null {
    for (let child = this.firstChild; child; child = child.getNextSibling()) {
      if (child instanceof CompoundDatum) {
        return child;
      }
    }
    return null;
  }

  /** @override */
  resetDesugars() {
    super.resetDesugars();
    this.forEachChild(child => child.resetDesugars());
  }

  forEachChild(callback: (datum: Datum) => void) {
    for (let cur = this.getFirstChild(); cur; cur = cur.getNextSibling()) {
      callback(cur);
    }
  }

  /**
   * Map isn't the best word, since the function returns an array but the children are represented
   * as a linked list.
   * @param f Function for transforming an individual child.
   * @return Array of transformed children.
   */
  mapChildren<T>(f: (datum: Datum) => T): T[] {
    const ans: T[] = [];
    for (let cur = this.getFirstChild(); cur; cur = cur.getNextSibling()) {
      ans.push(f(cur));
    }
    return ans;
  }

  /**
   * This penetrates quotations because it's used in quasiquote evaluation.
   * @param predicate Children passing this predicate are transformed according to the transform
   *     parameter.
   * @param transform Function that will transform children that pass the predicate.
   */
  replaceChildren(
      predicate: (datum: Datum) => boolean,
      transform: (datum: Datum) => Datum | null): this {

    for (let cur = this.firstChild, prev: Datum|null = null;
         cur;
         prev = cur, cur = cur!.getNextSibling()) {
      if (predicate(cur)) {
        const tmp = cur.getNextSibling();
        cur.setNextSibling(null);
        /* We have to assign to cur so prev will be set correctly
         in the next iteration. */
        if (cur = transform(cur)) {

          if (prev) {
            prev.setNextSibling(cur);
          } else {
            this.firstChild = cur;
          }

          /* If cur suddenly has a sibling, it must have been inserted
           by the transform. That is, the transform wants to insert
           multiple siblings in place of the single node. (Use case: in

           `(1 ,@(list 2 3) 4)

           the members of the sublist (2 3), not the sublist itself,
           should be inserted into the main list.)

           In this case we should skip ahead to the last sibling inserted
           by the transform in order to avoid accidentally running the
           transform on those newly-inserted siblings, which would
           presumably not be wanted. */
          if (cur.getNextSibling()) {
            cur = cur.lastSibling();
          }

          cur.setNextSibling(tmp);
        }

        /* If transform returned null, that means the current node
         should be spliced out of the list. */
        else {
          prev!.setNextSibling(tmp);
          cur = prev;
        }
      } else if (cur instanceof CompoundDatum) {
        cur.replaceChildren(predicate, transform);
      }
    }
    return this;
  }

  /**
   * Example:
   *
   * `(a `(b ,(+ x y) ,(foo ,(+ z w) d) e) f)
   *
   * should be decorated as
   *
   * `1(a `2(b ,2(+ x y) ,2(foo ,1(+ z w) d) e) f)
   *
   * @param qqLevel The level of quasiquotation.
   */
  setQuasiquotationLevel(qqLevel: number): this {
    this.forEachChild(child => {
      if (child instanceof CompoundDatum) {
        child.setQuasiquotationLevel(qqLevel);
      }
    });
    return this;
  }
}
