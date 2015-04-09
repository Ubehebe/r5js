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

goog.provide('r5js.ast.CompoundDatum');


goog.require('r5js.Datum');
goog.require('r5js.RenameHelper');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.Identifier');
goog.require('r5js.parse.Nonterminals');



/**
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.ast.CompoundDatum = function() {
  r5js.ast.CompoundDatum.base(this, 'constructor');

  /** @private {r5js.Datum} */ this.firstChild_ = null;

  /** @private {r5js.CdrHelper} */ this.cdrHelper_;

  /** @protected {number|undefined} */ this.qqLevel;
};
goog.inherits(r5js.ast.CompoundDatum, r5js.Datum);


/** @return {r5js.Datum} */
r5js.ast.CompoundDatum.prototype.getFirstChild = function() {
  return this.firstChild_;
};


/** @param {!r5js.Datum} firstChild */
r5js.ast.CompoundDatum.prototype.setFirstChild = function(firstChild) {
  this.firstChild_ = firstChild;
};


/** @return {number|undefined} */
r5js.ast.CompoundDatum.prototype.getQQLevel = function() {
  return this.qqLevel;
};


/**
 * Clears the first child.
 * TODO bl is this necessary?
 */
r5js.ast.CompoundDatum.prototype.clearFirstChild = function() {
  this.firstChild_ = null;
};


/** @override */
r5js.ast.CompoundDatum.prototype.clone = function(parent) {
  const ans = /** @type {!r5js.ast.CompoundDatum} */ (
      r5js.ast.CompoundDatum.base(this, 'clone', parent));
  if (this.firstChild_) {
    const buf = new r5js.SiblingBuffer();
    this.forEachChild(function(child) {
      buf.appendSibling(child.clone(ans));
    });
    ans.firstChild_ = buf.toSiblings();
  }
  return ans;
};


/**
 * TODO bl: this is intended to have the exact semantics of the library
 * procedure equal?, but I'm not sure that it does.
 * @param {!r5js.ast.CompoundDatum} other Datum to compare against.
 * @return {boolean}
 */
r5js.ast.CompoundDatum.prototype.isEqual = function(other) {
  let thisChild, otherChild;
  for (thisChild = this.firstChild_, otherChild = other.firstChild_;
      thisChild && otherChild;
      thisChild = thisChild.getNextSibling(),
      otherChild = otherChild.getNextSibling()) {
    if (thisChild instanceof r5js.ast.CompoundDatum &&
        otherChild instanceof r5js.ast.CompoundDatum &&
        !thisChild.isEqual(otherChild)) {
      return false;
    }
  }

  return !(thisChild || otherChild);
};


/** @override */
r5js.ast.CompoundDatum.prototype.fixParserSensitiveIds = function(helper) {
  if (this.hasParse(r5js.parse.Nonterminals.LAMBDA_EXPRESSION)) {
    this.fixParserSensitiveIdsLambda_(helper);
  } else if (this.hasParse(r5js.parse.Nonterminals.DEFINITION)) {
    this.fixParserSensitiveIdsDef_(helper);
  } else {
    for (let cur = this.firstChild_; cur; cur = cur.getNextSibling()) {
      cur.fixParserSensitiveIds(helper);
    }
  }
  r5js.ast.CompoundDatum.base(this, 'fixParserSensitiveIds', helper);
};


/**
 * TODO bl: document what this method does.
 * @param {!r5js.RenameHelper} helper A rename helper.
 * @private
 */
r5js.ast.CompoundDatum.prototype.fixParserSensitiveIdsLambda_ = function(
    helper) {
  const formalRoot = this.at(r5js.parse.Nonterminals.FORMALS);
  const newHelper = new r5js.RenameHelper(helper);

  if (formalRoot instanceof r5js.ast.Identifier) { // (lambda x ...)
    let id = formalRoot.getPayload();
    if (r5js.Datum.isParserSensitiveId(id)) {
      formalRoot.setPayload(newHelper.addRenameBinding(id));
    }
  } else { // (lambda (x y) ...) or (lambda (x . y) ...)
    (/** @type {!r5js.ast.CompoundDatum} */ (formalRoot)).forEachChild(
        function(child) {
          child = /** @type {!r5js.ast.Identifier} */ (child);
         let id = child.getPayload();
          if (r5js.Datum.isParserSensitiveId(id)) {
            child.setPayload(newHelper.addRenameBinding(id));
          }
        });
  }

  formalRoot.getNextSibling().fixParserSensitiveIds(newHelper);
};


/**
 * TODO bl: document what this method does.
 * @param {!r5js.RenameHelper} helper A rename helper.
 * @private
 */
r5js.ast.CompoundDatum.prototype.fixParserSensitiveIdsDef_ = function(helper) {
  const maybeVar = /** @type {r5js.ast.Identifier} */ (
      this.at(r5js.parse.Nonterminals.VARIABLE));
  let id;

  if (maybeVar) { // (define foo +)
    id = maybeVar.getPayload();
    if (r5js.Datum.isParserSensitiveId(id)) {
      maybeVar.setPayload(helper.addRenameBinding(id));
    }
  } else { // (define (foo x y) (+ x y))
    const vars = /** @type {!r5js.ast.CompoundDatum} */ (
        this.firstChild_.getNextSibling());
    const name = /** @type {!r5js.ast.Identifier} */ (vars.firstChild_);
    const newHelper = new r5js.RenameHelper(helper);
    for (let cur = name.getNextSibling(); cur; cur = cur.getNextSibling()) {
      cur = /** @type {!r5js.ast.Identifier} */ (cur);
      id = cur.getPayload();
      if (r5js.Datum.isParserSensitiveId(id)) {
        cur.setPayload(newHelper.addRenameBinding(id));
      }
    }
    vars.getNextSibling().fixParserSensitiveIds(newHelper);
    const namePayload = name.getPayload();
    if (r5js.Datum.isParserSensitiveId(namePayload)) {
      name.setPayload(helper.addRenameBinding(namePayload));
    }
  }
};


/**
 * @param {!r5js.parse.Nonterminal} type
 * @return {r5js.Datum}
 */
r5js.ast.CompoundDatum.prototype.at = function(type) {
  for (let cur = this.firstChild_; cur; cur = cur.getNextSibling()) {
    if (cur.peekParse() === type) {
      return cur;
    }
  }
  return null;
};


/**
 * @param {!r5js.CdrHelper} cdrHelper A cdr helper.
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.ast.CompoundDatum.prototype.setCdrHelper = function(cdrHelper) {
  this.cdrHelper_ = cdrHelper;
  return this;
};


/** @return {r5js.CdrHelper} The CdrHelper for this Datum, if one exists. */
r5js.ast.CompoundDatum.prototype.getCdrHelper = function() {
  return this.cdrHelper_;
};


/**
 * @return {r5js.ast.CompoundDatum} The first child of this datum that is
 * itself a list, or null if no such datum exists.
 */
r5js.ast.CompoundDatum.prototype.firstSublist = function() {
  for (let child = this.firstChild_; child; child = child.getNextSibling()) {
    if (child instanceof r5js.ast.CompoundDatum) {
      return child;
    }
  }
  return null;
};


/** @override */
r5js.ast.CompoundDatum.prototype.resetDesugars = function() {
  r5js.ast.CompoundDatum.base(this, 'resetDesugars');
  this.forEachChild(function(child) { child.resetDesugars(); });
};


/**
 * @param {function(this: T, !r5js.Datum)} callback
 * @param {T=} opt_context
 * @template T
 */
r5js.ast.CompoundDatum.prototype.forEachChild = function(
    callback, opt_context) {
  for (let cur = this.getFirstChild(); cur; cur = cur.getNextSibling()) {
    callback.call(opt_context, cur);
  }
};


/**
 * Map isn't the best word, since the function returns an array
 * but the children are represented as a linked list.
 * @param {function(this:SCOPE, !r5js.Datum):T} f Function for transforming
 * an individual child.
 * @param {SCOPE=} opt_context Optional receiver for f.
 * @return {!Array<T>} Array of transformed children.
 * @template SCOPE,T
 */
r5js.ast.CompoundDatum.prototype.mapChildren = function(f, opt_context) {
  const ans = [];
  for (let cur = this.getFirstChild(); cur; cur = cur.getNextSibling()) {
    ans.push(f.call(opt_context, cur));
  }
  return ans;
};


/**
 * This penetrates quotations because it's used in quasiquote evaluation.
 * @param {function(!r5js.Datum):boolean} predicate Children passing
 * this predicate are transformed according to the transform parameter.
 * @param {function(!r5js.Datum):r5js.Datum} transform Function
 * that will transform children that pass the predicate.
 * @return {!r5js.Datum} This object, for chaining.
 * @suppress {checkTypes} for setNextSibling(null) TODO bl fix
 */
r5js.ast.CompoundDatum.prototype.replaceChildren = function(
    predicate, transform) {

  for (var cur = this.firstChild_, prev;
       cur;
       prev = cur, cur = cur.getNextSibling()) {
    if (predicate(/** @type {!r5js.Datum} */(cur))) {
      const tmp = cur.getNextSibling();
      cur.setNextSibling(null);
      /* We have to assign to cur so prev will be set correctly
             in the next iteration. */
      if (cur = transform(/** @type {!r5js.Datum} */(cur))) {

        if (prev) {
          prev.setNextSibling(cur);
        } else {
          this.firstChild_ = cur;
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

        cur.setNextSibling(/** @type {!r5js.Datum} */ (tmp));
      }

    /* If transform returned null, that means the current node
             should be spliced out of the list. */
      else {
        prev.setNextSibling(/** @type {!r5js.Datum} */ (tmp));
        cur = prev;
      }
    } else if (cur instanceof r5js.ast.CompoundDatum) {
      cur.replaceChildren(predicate, transform);
    }
  }
  return this;
};


/**
 * Example:
 *
 * `(a `(b ,(+ x y) ,(foo ,(+ z w) d) e) f)
 *
 * should be decorated as
 *
 * `1(a `2(b ,2(+ x y) ,2(foo ,1(+ z w) d) e) f)
 *
 * @param {number} qqLevel The level of quasiquotation.
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.ast.CompoundDatum.prototype.setQuasiquotationLevel = function(qqLevel) {
  this.forEachChild(function(child) {
    if (child instanceof r5js.ast.CompoundDatum) {
      child.setQuasiquotationLevel(qqLevel);
    }
  });
  return this;
};
