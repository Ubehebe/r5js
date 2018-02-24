import {Nonterminal} from '../parse/nonterminals';
import {RenameHelper} from './rename_helper';

declare type DesugarFunc = (datum: Datum, env: IEnvironment) => any;

/**
 * TODO bl remove the "implements ObjectValue".
 * This illustrates the fundamental confusion between runtime values
 * and the AST.
*/
export class Datum implements ObjectValue {

  private nextSibling_: Datum|null = null;
  /** Only for last children */ private parent_: Datum|null = null;
  private readonly nonterminals_: Nonterminal[] = [];
  private readonly desugars_: DesugarFunc[] = [];
  private nextDesugar_: number = -1;
  private immutable_: boolean = false;

  constructor() {}

  setImmutable(): Datum /* for chaining */ {
    this.immutable_ = true;
    return this;
  }

  getParent(): Datum|null {
    return this.parent_;
  }

  setParent(parent: Datum) {
    this.parent_ = parent;
  }

  getNextSibling(): Datum|null {
    return this.nextSibling_;
  }

  setNextSibling(nextSibling: Datum|null /* TODO remove null */) {
    this.nextSibling_ = nextSibling;
  }

  isImmutable(): boolean {
    return this.immutable_;
  }

  clone(parent: Datum|null): Datum {

    /* Invariant: although cyclical Datum structures can be created by
     the programmer (through set-cdr!, etc.), they will never be cloned.
     They are created by mutation, i.e. once a value is already bound in an
     Environment, and once that happens, we never clone it again. */

    const ans = new (<typeof Datum>this.constructor)();

    if (this.parent_) {
      ans.parent_ = this.parent_;
    }
    // We only need the parent_ pointer on the last sibling.
    if (!this.nextSibling_) {
      ans.parent_ = parent;
    }
    if (this.immutable_) {
      ans.immutable_ = true;
    }

    return ans;
  }

  setParse(type: Nonterminal) {
    this.nonterminals_.push(type);
  }

  setDesugar(desugarFunc: (datum: Datum, env: IEnvironment) => any /* TODO can't use DesguarFunc typedef */) {
    this.desugars_.push(desugarFunc);
    ++this.nextDesugar_;
  }

  peekParse(): Nonterminal|null {
    const len = this.nonterminals_.length;
    return len > 0 ? this.nonterminals_[len - 1] : null;
  }

  /** @returns true iff this Datum parses as the given nonterminal. */
  protected hasParse(nonterminal: Nonterminal): boolean {
    if (this.nonterminals_) {
      const len = this.nonterminals_.length;
      for (let i = 0; i < len; ++i) {
        if (this.nonterminals_[i] === nonterminal) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * @return {boolean} True if this datum represents an improper list.
   * TODO bl: remove. Callers shouldn't be dispatching on this. Rather, the
   * list/dotted list behavior differences should be built into the Datum
   * subclasses.
   */
  isImproperList(): boolean {
    return false;
  }

  /**
   * TODO bl: document why you would call this method.
   */
  resetDesugars() {
    if (this.nextDesugar_ === -1) {
      this.nextDesugar_ += this.desugars_.length;
    }
  }

  /**
   * @param forceContinuationWrapper TODO bl document.
   * @return {!Datum|!ProcCallLike|!r5js.Subtransformer|!r5js.Macro|null} TODO update TypeScript type.
   */
  desugar(env: IEnvironment, forceContinuationWrapper:boolean=false): any {
    const desugarFn = (this.nextDesugar_ >= 0)
        ? this.desugars_[this.nextDesugar_--]
        : null;
    let ans = desugarFn ? desugarFn(this, env) : this;
    if (forceContinuationWrapper && (ans instanceof Datum)) {
      ans = ans.toProcCallLike();
    }
    return ans;
  }

  sequence(env: IEnvironment): ProcCallLike|undefined {
    let first: ProcCallLike|undefined;
    let desugared;
    let curEnd: ProcCallLike|undefined;
    for (let cur:Datum|null = this; cur; cur = cur.nextSibling_) {
      if (desugared = cur.desugar(env)) {

        /* Nodes that have no desugar functions (for example, variables
         and literals) desugar as themselves. Sometimes this is OK
         (for example in Datum.sequenceOperands), but here we need to be
         able to connect the Continuable objects correctly, so we
         wrap them. */
        const desugaredProcCallLike = /** @type {!ProcCallLike} */ (
            desugared instanceof Datum
                ? desugared.toProcCallLike()
                : desugared);

        if (!first) {
          first = desugaredProcCallLike;
        } else if (curEnd) {
          curEnd.setNext(desugaredProcCallLike);
        }

        curEnd = getLastProcCallLike(desugaredProcCallLike);
      }
    }

    return first;
  }

  /**
   * @returns Whether the two datums are equivalent in the sense of eqv?
   * For most kinds of datum, this means reference equality.
   * @see R5RS 6.1
   */
  eqv(other: Datum): boolean {
    return this === other;
  }

  /**
   * Datums representing identifiers, strings, and characters
   * all have payloads of type string. If they all unwrapped as JavaScript
   * strings, it would be impossible to re-wrap them correctly
   * (noninjective mapping). We choose to store identifiers unwrapped
   * because they're expected to be more common than the other two.
   *
   * Environment specifiers cannot be unwrapped to their Environment
   * payloads because Environment values in Environments already have
   * a meaning, namely, a redirect to look up the name in some other
   * Environment.
   *
   * Finally, the vector stuff may need to be overhauled.
   */
  unwrap(): any /* TODO */ {
    return this;
  }

  /**
   * @returns the last sibling of this Datum, or this Datum if it's
   * the last sibling.
   */
  lastSibling(): Datum {
    return this.nextSibling_ ? this.nextSibling_.lastSibling() : this;
  }

  /** TODO bl: document what this method does. */
  fixParserSensitiveIds(helper: RenameHelper) {
    if (this.nextSibling_) {
      this.nextSibling_.fixParserSensitiveIds(helper);
    }
  }

  toProcCallLike(): ProcCallLike {
    return new DatumShim(this);
  }
}

/**
 * TODO: this should be in its own module. It's lumped here to work around
 * a Closure Compiler optimization bug that was causing two (different) copies of ProcCallLike
 * to be written to the compiled output. See the commit description in the commit that added this
 * comment for more information.
 *
 * Hopefully, this will be obsolete once the TypeScript migration is finished.
 */
export abstract class ProcCallLike {

  protected resultName_: string;
  protected next_: ProcCallLike|null;
  protected env_: IEnvironment|null;

  constructor(lastResultName:string=`@${counter++}`) {
    this.resultName_ = lastResultName;
    this.next_ = null;
    this.env_ = null;
  }

  /**
   * parserProvider Function that will return a new Parser for the given Datum when called.
   */
  abstract evalAndAdvance(resultStruct: ProcCallResult /* TODO */, env: IEnvironment, parserProvider: (x: any) => any /* TODO */);

  getResultName(): string {
    return this.resultName_;
  }

  /** TODO bl remove. */
  setResultName(resultName: string) {
    this.resultName_ = resultName;
  }

  setStartingEnv(env: IEnvironment) {
    this.env_ = env;
  }

  getEnv(): IEnvironment|null {
    return this.env_;
  }

  /** Clears the current environment. TODO bl not well understood. */
  clearEnv() {
    this.env_ = null;
  }

  getNext(): ProcCallLike|null {
    return this.next_;
  }

  setNext(next: ProcCallLike) {
    this.next_ = next;
  }

  bindResult(val: Value) {
    /* If the next procedure call already has an environment,
     bind the result there. Otherwise, bind it in the current
     environment; it will be carried forward by the EnvBuffer. */
    const envToUse = (this.next_ && this.next_.getEnv && this.next_.getEnv()) || this.env_;
    envToUse && envToUse.addBinding && envToUse.addBinding(this.resultName_, val);
  }
}

let counter: number = 0;

export function appendProcCallLike(procCallLike: ProcCallLike, next: ProcCallLike) {
  getLastProcCallLike(procCallLike).setNext(next);
}

export function getLastProcCallLike(procCallLike: ProcCallLike): ProcCallLike {
  const maybeNext = procCallLike.getNext();
  return maybeNext ? getLastProcCallLike(maybeNext) : procCallLike;
}

export class ProcCallResult {
  setNext(procCallLike: ProcCallLike) {}
  setValue(value: Value) {}
  getNextProcCallLike(): ProcCallLike|null {
    return null;
  }
}

class DatumShim extends ProcCallLike {

  private readonly firstOperand: Datum;

  constructor(payload: Datum) {
    super();
    this.firstOperand = payload;
  }

  /** @override */
  evalAndAdvance(resultStruct: ProcCallResult, env: IEnvironment, parserProvider: (x: any) => any /* TODO */) {
    this.bindResult(this.firstOperand);
    resultStruct.setValue(this.firstOperand);
    const nextContinuable = this.getNext();
    if (nextContinuable) {
      resultStruct.setNext(nextContinuable);
    }
  }
}

/**
 * According to the R5RS grammar, a sequence of zero datums is a valid program.
 * This object is used to prevent the interpreter from returning null
 * in contexts where that might erroneously be interpreted as an error.
 */
export const VACUOUS_PROGRAM = new Datum();

export const UNSPECIFIED_VALUE: Datum = new Datum();

