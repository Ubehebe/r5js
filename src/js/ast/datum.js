goog.provide('r5js.Datum');

goog.require('r5js.ProcCallLike');
goog.require('r5js.parse.Terminals');
goog.require('r5js.runtime.ObjectValue');

/** @typedef {function(!r5js.Datum, !r5js.IEnvironment):
* (!r5js.Datum|!r5js.ProcCallLike|!r5js.ITransformer|!r5js.Macro|null)}
 * TODO bl: narrow this typedef.
 */
r5js.DesugarFunc;



/**
 * TODO bl remove the "implements ObjectValue".
 * This illustrates the fundamental confusion between runtime values
 * and the AST.
 */
r5js.Datum = /** @implements {r5js.runtime.ObjectValue} */ class {
  constructor() {
    /** @private {r5js.Datum} */
    this.nextSibling_ = null;

    /**
     * Only for last children.
     * @private {r5js.Datum}
     */
    this.parent_ = null;

    /** @const @private {!Array<!r5js.parse.Nonterminal>} */
    this.nonterminals_ = [];

    /** @const @private {!Array<!r5js.DesugarFunc>} */
    this.desugars_ = [];

    /** @private {number} */
    this.nextDesugar_ = -1;

    /** @private */ this.immutable_ = false;
  }

  /**
   * @return {!r5js.Datum} This object, for chaining.
   */
  setImmutable() {
    this.immutable_ = true;
    return this;
  }

  /** @return {r5js.Datum} */
  getParent() {
    return this.parent_;
  }

  /** @param {!r5js.Datum} parent */
  setParent(parent) {
    this.parent_ = parent;
  }

  /** @return {r5js.Datum} */
  getNextSibling() {
    return this.nextSibling_;
  }

  /** @param {!r5js.Datum} nextSibling */
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
   * @param {r5js.Datum} parent Datum to use for the parent of the clone, if any.
   * @return {!r5js.Datum} A new clone of this Datum object.
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

  /** @param {!r5js.parse.Nonterminal} type */
  setParse(type) {
    this.nonterminals_.push(type);
  }

  /** @param {!r5js.DesugarFunc} desugarFunc */
  setDesugar(desugarFunc) {
    this.desugars_.push(desugarFunc);
    ++this.nextDesugar_;
  }

  /** @return {?r5js.parse.Nonterminal} */
  peekParse() {
    const len = this.nonterminals_.length;
    return len > 0 ? this.nonterminals_[len - 1] : null;
  }

  /**
   * @param {!r5js.parse.Nonterminal} nonterminal
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
   * @param {!r5js.IEnvironment} env TODO bl.
   * @param {boolean=} opt_forceContinuationWrapper TODO bl document.
   * @return {!r5js.Datum|!r5js.ProcCallLike|!r5js.ITransformer|!r5js.Macro|null}
   * @suppress {checkTypes} TODO bl
   */
  desugar(env, opt_forceContinuationWrapper) {
    const desugarFn = (this.desugars_ && this.nextDesugar_ >= 0)
        ? this.desugars_[this.nextDesugar_--]
        : null;
    let ans = desugarFn ? desugarFn(this, env) : this;
    if (opt_forceContinuationWrapper && (ans instanceof r5js.Datum)) {
      ans = ans.toProcCallLike();
    }
    return ans;
  }

  /**
   * @param {!r5js.IEnvironment} env TODO bl.
   * @return {r5js.ProcCallLike}
   */
  sequence(env) {
    /** @type {r5js.ProcCallLike} */ let first = null;
    let desugared;
    /** @type {r5js.ProcCallLike} */ let curEnd = null;
    for (let cur = this; cur; cur = cur.nextSibling_) {
      if (desugared = cur.desugar(env)) {

        /* Nodes that have no desugar functions (for example, variables
         and literals) desugar as themselves. Sometimes this is OK
         (for example in Datum.sequenceOperands), but here we need to be
         able to connect the Continuable objects correctly, so we
         wrap them. */
        const desugaredProcCallLike = /**@type {!r5js.ProcCallLike} */ (
            desugared instanceof r5js.Datum
                ? desugared.toProcCallLike()
                : desugared);

        if (!first) {
          first = desugaredProcCallLike;
        } else if (curEnd) {
          curEnd.setNext(desugaredProcCallLike);
        }

        curEnd = r5js.ProcCallLike.getLast(desugaredProcCallLike);
      }
    }

    return first;
  }

  /**
   * @param {!r5js.Datum} other
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
   * @return {!r5js.Datum} The last sibling of this Datum, or this Datum if it's
   * the last sibling.
   */
  lastSibling() {
    return this.nextSibling_ ? this.nextSibling_.lastSibling() : this;
  }

  /**
   * TODO bl: document what this method does.
   * @param {!r5js.RenameHelper} helper A rename helper.
   */
  fixParserSensitiveIds(helper) {
    if (this.nextSibling_) {
      this.nextSibling_.fixParserSensitiveIds(helper);
    }
  }

  /** @return {!r5js.ProcCallLike} */
  toProcCallLike() {
    return new r5js.DatumShim_(this);
  }
};


/**
 * @param {r5js.Datum} payload
 * @param {string=} opt_continuationName Optional name of the continuation.
 * @extends {r5js.ProcCallLike}
 * @struct
 * @constructor
 * @private
 */
r5js.DatumShim_ = function (payload, opt_continuationName) {
  r5js.DatumShim_.base(this, 'constructor', opt_continuationName);
  /** @const @private */ this.firstOperand_ = payload;
};
goog.inherits(r5js.DatumShim_, r5js.ProcCallLike);

/** @override */
r5js.DatumShim_.prototype.evalAndAdvance = function (resultStruct, env, parserProvider) {
  if (this.firstOperand_ !== null) {
    this.bindResult(this.firstOperand_);
    resultStruct.setValue(this.firstOperand_);
  }

  const nextContinuable = this.getNext();

  if (nextContinuable) {
    resultStruct.setNext(nextContinuable);
  }
};
