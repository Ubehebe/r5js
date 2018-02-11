goog.module('r5js.Datum');

const IEnvironment = goog.require('r5js.IEnvironment');
const RenameHelper = goog.require('r5js.RenameHelper');
const {Nonterminal} = goog.require('r5js.parse.Nonterminals');
const {ProcCallLike} = goog.require('r5js.ProcCallLike');

/** @typedef {function(!Datum, !IEnvironment): ?} */ let DesugarFunc;

/**
 * TODO bl remove the "implements ObjectValue".
 * This illustrates the fundamental confusion between runtime values
 * and the AST.
 * @implements {ObjectValue}
*/
class Datum {
  constructor() {
    /** @private {?Datum} */ this.nextSibling_ = null;

    /**
     * Only for last children.
     * @private {?Datum}
     */
    this.parent_ = null;

    /** @const @private {!Array<!Nonterminal>} */
    this.nonterminals_ = [];

    /** @const @private {!Array<!DesugarFunc>} */
    this.desugars_ = [];

    /** @private {number} */
    this.nextDesugar_ = -1;

    /** @private */ this.immutable_ = false;
  }

  /** @return {!Datum} This object, for chaining. */
  setImmutable() {
    this.immutable_ = true;
    return this;
  }

  /** @return {?Datum} */
  getParent() {
    return this.parent_;
  }

  /** @param {!Datum} parent */
  setParent(parent) {
    this.parent_ = parent;
  }

  /** @return {?Datum} */
  getNextSibling() {
    return this.nextSibling_;
  }

  /** @param {?Datum} nextSibling */
  setNextSibling(nextSibling) {
    this.nextSibling_ = nextSibling;
  }

  /**
   * @return {boolean} True iff {@link #setImmutable} has been called
   * on this Datum.
   */
  isImmutable() {
    return this.immutable_;
  }

  /**
   * @param {?Datum} parent Datum to use for the parent of the clone, if any.
   * @return {!Datum} A new clone of this Datum object.
   */
  clone(parent) {

    /* Invariant: although cyclical Datum structures can be created by
     the programmer (through set-cdr!, etc.), they will never be cloned.
     They are created by mutation, i.e. once a value is already bound in an
     Environment, and once that happens, we never clone it again. */

    const ans = new this.constructor();

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

  /** @param {!Nonterminal} type */
  setParse(type) {
    this.nonterminals_.push(type);
  }

  /** @param {!DesugarFunc} desugarFunc */
  setDesugar(desugarFunc) {
    this.desugars_.push(desugarFunc);
    ++this.nextDesugar_;
  }

  /** @return {?Nonterminal} */
  peekParse() {
    const len = this.nonterminals_.length;
    return len > 0 ? this.nonterminals_[len - 1] : null;
  }

  /**
   * @param {!Nonterminal} nonterminal
   * @return {boolean} True iff this Datum parses as the given nonterminal.
   * @protected
   */
  hasParse(nonterminal) {
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
  isImproperList() {
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
   * @param {!IEnvironment} env TODO bl.
   * @param {boolean=} forceContinuationWrapper TODO bl document.
   * @return {!Datum|!ProcCallLike|!r5js.Subtransformer|!r5js.Macro|null}
   */
  desugar(env, forceContinuationWrapper=false) {
    const desugarFn = (this.nextDesugar_ >= 0)
        ? this.desugars_[this.nextDesugar_--]
        : null;
    let ans = desugarFn ? desugarFn(this, env) : this;
    if (forceContinuationWrapper && (ans instanceof Datum)) {
      ans = ans.toProcCallLike();
    }
    return ans;
  }

  /**
   * @param {!IEnvironment} env TODO bl.
   * @return {?ProcCallLike}
   */
  sequence(env) {
    /** @type {?ProcCallLike} */ let first;
    let desugared;
    /** @type {?ProcCallLike} */ let curEnd;
    for (let cur = this; cur; cur = cur.nextSibling_) {
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

        curEnd = ProcCallLike.getLast(desugaredProcCallLike);
      }
    }

    return first;
  }

  /**
   * @param {!Datum} other
   * @return {boolean} Whether the two datums are equivalent in the sense of eqv?
   * For most kinds of datum, this means reference equality.
   * @see R5RS 6.1
   */
  eqv(other) {
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
   * @return {?} TODO bl.
   */
  unwrap() {
    return this;
  }

  /**
   * @return {!Datum} The last sibling of this Datum, or this Datum if it's
   * the last sibling.
   */
  lastSibling() {
    return this.nextSibling_ ? this.nextSibling_.lastSibling() : this;
  }

  /**
   * TODO bl: document what this method does.
   * @param {!RenameHelper} helper A rename helper.
   */
  fixParserSensitiveIds(helper) {
    if (this.nextSibling_) {
      this.nextSibling_.fixParserSensitiveIds(helper);
    }
  }

  /** @return {!ProcCallLike} */
  toProcCallLike() {
    return new DatumShim(this);
  }
}

class DatumShim extends ProcCallLike {
  /** @param {!Datum} payload */
  constructor(payload) {
    super();
    /** @const @private */ this.firstOperand_ = payload;
  }

  /** @override */
  evalAndAdvance(resultStruct, env, parserProvider) {
    if (this.firstOperand_ !== null) {
      this.bindResult(this.firstOperand_);
      resultStruct.setValue(this.firstOperand_);
    }

    const nextContinuable = this.getNext();

    if (nextContinuable) {
      resultStruct.setNext(nextContinuable);
    }
  }
}

exports = Datum;
