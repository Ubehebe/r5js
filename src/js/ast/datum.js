/* Copyright 2011-2014 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

goog.provide('r5js.Datum');
goog.provide('r5js.VACUOUS_PROGRAM');
goog.provide('r5js.ast.Literal');


goog.require('r5js.ProcCallLike');
goog.require('r5js.parse.Terminals');
// TODO bl circular dependency goog.require('r5js.newIdShim');


/** @typedef {function(!r5js.Datum, !r5js.IEnvironment):
* (!r5js.Datum|!r5js.ProcCallLike|!r5js.ITransformer|!r5js.Macro|null)}
 * TODO bl: narrow this typedef.
 */
r5js.DesugarFunc;



/**
 * @implements {r5js.runtime.ObjectValue} TODO bl remove.
 * This illustrates the fundamental confusion between runtime values
 * and the AST.
 * @struct
 * @constructor
 */
r5js.Datum = function() {
  /** @private {r5js.Datum} */
  this.nextSibling_ = null;

  /**
     * Only for last children.
     * @private {r5js.Datum}
     */
  this.parent_ = null;

  /** @const @private {!Array.<!r5js.parse.Nonterminal>} */
  this.nonterminals_ = [];

  /** @const @private {!Array.<!r5js.DesugarFunc>} */
  this.desugars_ = [];

  /** @private {number} */
  this.nextDesugar_ = -1;

  /** @private */ this.immutable_ = false;
};


/**
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.Datum.prototype.setImmutable = function() {
  this.immutable_ = true;
  return this;
};


/** @return {r5js.Datum} */
r5js.Datum.prototype.getParent = function() {
  return this.parent_;
};


/** @param {!r5js.Datum} parent */
r5js.Datum.prototype.setParent = function(parent) {
  this.parent_ = parent;
};


/** @return {r5js.Datum} */
r5js.Datum.prototype.getNextSibling = function() {
  return this.nextSibling_;
};


/** @param {!r5js.Datum} nextSibling */
r5js.Datum.prototype.setNextSibling = function(nextSibling) {
  this.nextSibling_ = nextSibling;
};


/**
 * @return {boolean} True iff {@link r5js.Datum.setImmutable} has been called
 * on this Datum.
 */
r5js.Datum.prototype.isImmutable = function() {
  return this.immutable_;
};


/**
 * @param {r5js.Datum} parent Datum to use for the parent of the clone, if any.
 * @return {!r5js.Datum} A new clone of this Datum object.
 */
r5js.Datum.prototype.clone = function(parent) {

  /* Invariant: although cyclical Datum structures can be created by
     the programmer (through set-cdr!, etc.), they will never be cloned.
     They are created by mutation, i.e. once a value is already bound in an
     Environment, and once that happens, we never clone it again. */

  var ans = new this.constructor();

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
};


/** @param {!r5js.parse.Nonterminal} type */
r5js.Datum.prototype.setParse = function(type) {
  this.nonterminals_.push(type);
};


/** @param {!r5js.DesugarFunc} desugarFunc */
r5js.Datum.prototype.setDesugar = function(desugarFunc) {
  this.desugars_.push(desugarFunc);
  ++this.nextDesugar_;
};


/** @return {?r5js.parse.Nonterminal} */
r5js.Datum.prototype.peekParse = function() {
  var len = this.nonterminals_.length;
  return len > 0 ? this.nonterminals_[len - 1] : null;
};


/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @return {boolean} True iff this Datum parses as the given nonterminal.
 * @protected
 */
r5js.Datum.prototype.hasParse = function(nonterminal) {
  if (this.nonterminals_) {
    var len = this.nonterminals_.length;
    for (var i = 0; i < len; ++i) {
      if (this.nonterminals_[i] === nonterminal) {
        return true;
      }
    }
  }
  return false;
};


/**
 * @return {boolean} True if this datum represents an improper list.
 * TODO bl: remove. Callers shouldn't be dispatching on this. Rather, the
 * list/dotted list behavior differences should be built into the Datum
 * subclasses.
 */
r5js.Datum.prototype.isImproperList = function() {
  return false;
};


/**
 * TODO bl: document why you would call this method.
 */
r5js.Datum.prototype.resetDesugars = function() {
  if (this.nextDesugar_ === -1) {
    this.nextDesugar_ += this.desugars_.length;
  }
};


/**
 * @param {!r5js.IEnvironment} env TODO bl.
 * @param {boolean=} opt_forceContinuationWrapper TODO bl document.
 * @return {!r5js.Datum|!r5js.ProcCallLike|!r5js.ITransformer|!r5js.Macro|null}
 * @suppress {checkTypes} TODO bl
 */
r5js.Datum.prototype.desugar = function(env, opt_forceContinuationWrapper) {
  var desugarFn = (this.desugars_ && this.nextDesugar_ >= 0) ?
      this.desugars_[this.nextDesugar_--] : null;
  var ans = desugarFn ? desugarFn(this, env) : this;
  if (opt_forceContinuationWrapper && (ans instanceof r5js.Datum)) {
    ans = new r5js.IdShim(ans);
  }
  return ans;
};


/**
 * @param {!r5js.IEnvironment} env TODO bl.
 * @return {r5js.ProcCallLike}
 */
r5js.Datum.prototype.sequence = function(env) {
  /** @type {r5js.ProcCallLike} */ var first = null;
  var desugared;
  /** @type {r5js.ProcCallLike} */ var curEnd;
  for (var cur = this; cur; cur = cur.nextSibling_) {
    if (desugared = cur.desugar(env)) {

      /* Nodes that have no desugar functions (for example, variables
             and literals) desugar as themselves. Sometimes this is OK
             (for example in Datum.sequenceOperands), but here we need to be
             able to connect the Continuable objects correctly, so we
             wrap them. */
      var desugaredProcCallLike = /**@type {!r5js.ProcCallLike} */ (
          desugared instanceof r5js.Datum ?
          new r5js.IdShim(desugared) :
          desugared);

      if (!first) {
        first = desugaredProcCallLike;
      } else if (curEnd) {
        curEnd.setNext(desugaredProcCallLike);
      }

      curEnd = r5js.ProcCallLike.getLast(desugaredProcCallLike);
    }
  }

  return first;
};



/**
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.ast.Literal = function() {
  goog.base(this);
};
goog.inherits(r5js.ast.Literal, r5js.Datum);


/**
 * @param {!r5js.Datum} other
 * @return {boolean} Whether the two datums are equivalent in the sense of eqv?
 * For most kinds of datum, this means reference equality.
 * @see R5RS 6.1
 */
r5js.Datum.prototype.eqv = function(other) {
  return this === other;
};


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
r5js.Datum.prototype.unwrap = function() {
  return this;
};


/**
 * @return {!r5js.Datum} The last sibling of this Datum, or this Datum if it's
 * the last sibling.
 */
r5js.Datum.prototype.lastSibling = function() {
  return this.nextSibling_ ? this.nextSibling_.lastSibling() : this;
};


/** @return {string} */
function newCpsName() {
  /* TODO bl: goog.getUid requires an object parameter, so this method
       creates a throwaway object. Requiring this function to take an object
       parameter could reduce garbage. */
  return r5js.Datum.CPS_PREFIX_ + goog.getUid(new Object());
}


/** @return {string} */
function newAnonymousLambdaName() {
  /* TODO bl: goog.getUid requires an object parameter, so this method
    creates a throwaway object. Requiring this function to take an object
    parameter could reduce garbage. */
  return r5js.Datum.PROC_PREFIX_ + goog.getUid(new Object());
}


/**
 * Not a valid identifier prefix so we can easily tell these apart.
 * @const {string}
 * @private
 */
r5js.Datum.CPS_PREFIX_ = '@';


/** @const @private {string} */
r5js.Datum.PROC_PREFIX_ = 'proc';


/**
 * See comments at the top of Parser.
 * @param {string} name identifier name to check.
 * @return {boolean} True iff the given name is parser-sensitive.
 */
function isParserSensitiveId(name) {
  switch (name) {
    case r5js.parse.Terminals.BEGIN:
    case r5js.parse.Terminals.DEFINE:
    case r5js.parse.Terminals.DEFINE_SYNTAX:
    case r5js.parse.Terminals.IF:
    case r5js.parse.Terminals.LAMBDA:
    case r5js.parse.Terminals.LET_SYNTAX:
    case r5js.parse.Terminals.LETREC_SYNTAX:
    case r5js.parse.Terminals.QUASIQUOTE:
    case r5js.parse.Terminals.QUOTE:
    case r5js.parse.Terminals.SET:
    case r5js.parse.Terminals.UNQUOTE:
    case r5js.parse.Terminals.UNQUOTE_SPLICING:
      return true;
    default:
      return false;
  }
}


/**
 * TODO bl: document what this method does.
 * @param {!r5js.RenameHelper} helper A rename helper.
 */
r5js.Datum.prototype.fixParserSensitiveIds = function(helper) {
  if (this.nextSibling_) {
    this.nextSibling_.fixParserSensitiveIds(helper);
  }
};


/**
 * According to the R5RS grammar, a sequence of zero datums is a valid program.
 * This object is used to prevent the interpreter from returning null
 * in contexts where that might erroneously be interpreted as an error.
 * @const
 */
r5js.VACUOUS_PROGRAM = new r5js.Datum();
