goog.provide('r5js.ast.CompoundDatum');


goog.require('r5js.Datum');
goog.require('r5js.SiblingBuffer');



/**
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.ast.CompoundDatum = function() {
  goog.base(this);
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


/**
 * Clears the first child.
 * TODO bl is this necessary?
 */
r5js.ast.CompoundDatum.prototype.clearFirstChild = function() {
  this.firstChild_ = null;
};


/** @override */
r5js.ast.CompoundDatum.prototype.clone = function(parent) {
  var ans = goog.base(this, 'clone', parent);
  if (this.firstChild_) {
    var buf = new r5js.SiblingBuffer();
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
 * @param {!r5js.Datum} other Datum to compare against.
 * @return {boolean}
 */
r5js.ast.CompoundDatum.prototype.isEqual = function(other) {
  var thisChild, otherChild;
  for (thisChild = this.firstChild_, otherChild = other.firstChild_;
      thisChild && otherChild;
      thisChild = thisChild.getNextSibling(),
      otherChild = otherChild.getNextSibling()) {
    if (thisChild instanceof r5js.ast.CompoundDatum &&
        !thisChild.isEqual(otherChild)) {
      return false;
    }
  }

  return !(thisChild || otherChild);
};


/**
 * @return {r5js.ast.CompoundDatum} The first child of this datum that is
 * itself a list, or null if no such datum exists.
 */
r5js.ast.CompoundDatum.prototype.firstSublist = function() {
  for (var child = this.firstChild_; child; child = child.getNextSibling()) {
    if (child instanceof r5js.ast.CompoundDatum) {
      return child;
    }
  }
  return null;
};


/** @override */
r5js.ast.CompoundDatum.prototype.resetDesugars = function() {
  goog.base(this, 'resetDesugars');
  this.forEachChild(function(child) { child.resetDesugars(); });
};


/**
 * @param {function(this: T, !r5js.Datum)} callback
 * @param {T=} opt_context
 * @template T
 */
r5js.ast.CompoundDatum.prototype.forEachChild = function(
    callback, opt_context) {
  for (var cur = this.getFirstChild(); cur; cur = cur.getNextSibling()) {
    callback.call(opt_context, cur);
  }
};


/**
 * Map isn't the best word, since the function returns an array
 * but the children are represented as a linked list.
 * @param {function(this:SCOPE, !r5js.Datum):T} f Function for transforming
 * an individual child.
 * @param {SCOPE=} opt_context Optional receiver for f.
 * @return {!Array.<T>} Array of transformed children.
 * @template SCOPE,T
 */
r5js.ast.CompoundDatum.prototype.mapChildren = function(f, opt_context) {
  var ans = [];
  for (var cur = this.getFirstChild(); cur; cur = cur.getNextSibling()) {
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
      var tmp = cur.getNextSibling();
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
