import {Nonterminal} from '../parse/nonterminals';
import {RenameHelper} from './rename_helper';
import {Environment} from "../runtime/environment";
import {ObjectValue} from "../value";
import {ProcCallLike, ProcCallResult} from "./proc_call_like";

/**
 * TODO: `T` should be `T extends Datum`. But there are a couple crazy implementations:
 * a DesugarFunc<string> (OneTerminal) and a DesugarFunc<Nonterminal> (OneNonterminal).
 * These should be investigated and removed.
 */
export type DesugarFunc<T> = (datum: T, env: Environment) => any /* TODO tighten return type */;

/**
 * TODO bl remove the "implements ObjectValue".
 * This illustrates the fundamental confusion between runtime values
 * and the AST.
*/
export class Datum implements ObjectValue {

  private nextSibling: Datum|null = null;
  /** Only for last children */ private parent: Datum|null = null;
  private readonly nonterminals: Nonterminal[] = [];
  private readonly desugars: DesugarFunc<Datum>[] = [];
  private nextDesugar: number = -1;
  private immutable: boolean = false;

  constructor() {}

  setImmutable(): this {
    this.immutable = true;
    return this;
  }

  getParent(): Datum|null {
    return this.parent;
  }

  setParent(parent: Datum) {
    this.parent = parent;
  }

  getNextSibling(): Datum|null {
    return this.nextSibling;
  }

  setNextSibling(nextSibling: Datum|null /* TODO remove null */) {
    this.nextSibling = nextSibling;
  }

  isImmutable(): boolean {
    return this.immutable;
  }

  clone(parent: Datum | undefined = undefined): Datum {

    // Invariant: although cyclical Datum structures can be created by the programmer
    // (through set-cdr!, etc.), they will never be cloned. They are created by mutation, i.e. once
    // a value is already bound in an Environment, and once that happens, we never clone it again.

    const ans = new (<typeof Datum>this.constructor)();

    if (this.parent) {
      ans.parent = this.parent;
    }
    // We only need the parent_ pointer on the last sibling.
    if (!this.nextSibling && parent) {
      ans.parent = parent;
    }
    if (this.immutable) {
      ans.immutable = true;
    }

    return ans;
  }

  setParse(type: Nonterminal) {
    this.nonterminals.push(type);
  }

  setDesugar(desugarFunc: (datum: Datum, env: Environment) => any /* TODO can't use DesguarFunc typedef */) {
    this.desugars.push(desugarFunc);
    ++this.nextDesugar;
  }

  peekParse(): Nonterminal|null {
    const len = this.nonterminals.length;
    return len > 0 ? this.nonterminals[len - 1] : null;
  }

  /** @returns true iff this Datum parses as the given nonterminal. */
  protected hasParse(nonterminal: Nonterminal): boolean {
    if (this.nonterminals) {
      const len = this.nonterminals.length;
      for (let i = 0; i < len; ++i) {
        if (this.nonterminals[i] === nonterminal) {
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
    if (this.nextDesugar === -1) {
      this.nextDesugar += this.desugars.length;
    }
  }

  /**
   * @param forceContinuationWrapper TODO bl document.
   * @return {!Datum|!ProcCallLike|!r5js.Subtransformer|!r5js.Macro|null} TODO update TypeScript type.
   */
  desugar(env: Environment, forceContinuationWrapper:boolean=false): any {
    const desugarFn = (this.nextDesugar >= 0)
        ? this.desugars[this.nextDesugar--]
        : null;
    let ans = desugarFn ? desugarFn(this, env) : this;
    if (forceContinuationWrapper && (ans instanceof Datum)) {
      ans = ans.toProcCallLike();
    }
    return ans;
  }

  sequence(env: Environment): ProcCallLike|undefined {
    let first: ProcCallLike|undefined;
    let desugared;
    let curEnd: ProcCallLike|undefined;
    for (let cur:Datum|null = this; cur; cur = cur.nextSibling) {
      if (desugared = cur.desugar(env)) {
        // Nodes that have no desugar functions (for example, variables and literals) desugar as
        // themselves. Sometimes this is OK (for example in Datum.sequenceOperands), but here we need
        // to be able to connect the Continuable objects correctly, so we wrap them.
        const desugaredProcCallLike = desugared instanceof Datum
            ? desugared.toProcCallLike()
            : desugared;
        if (!first) {
          first = desugaredProcCallLike;
        } else if (curEnd) {
          curEnd.setNext(desugaredProcCallLike);
        }
        curEnd = desugaredProcCallLike.getLast();
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
   * @returns the last sibling of this Datum, or this Datum if it's
   * the last sibling.
   */
  lastSibling(): Datum {
    return this.nextSibling ? this.nextSibling.lastSibling() : this;
  }

  /** TODO bl: document what this method does. */
  fixParserSensitiveIds(helper: RenameHelper) {
    if (this.nextSibling) {
      this.nextSibling.fixParserSensitiveIds(helper);
    }
  }

  toProcCallLike(): ProcCallLike {
    return new DatumShim(this);
  }
}

class DatumShim extends ProcCallLike {

  private readonly firstOperand: Datum;

  constructor(payload: Datum) {
    super();
    this.firstOperand = payload;
  }

  /** @override */
  evalAndAdvance(resultStruct: ProcCallResult, env: Environment, parserProvider: (x: any) => any /* TODO */) {
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

