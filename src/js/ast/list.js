goog.provide('r5js.ast.List');

goog.require('r5js.IPair');
// TODO bl circular dependency goog.require('r5js.ast.DottedList');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.CompoundDatum');

/**
 * @param {r5js.Datum} firstChild
 * @implements {r5js.IPair}
 * @extends {r5js.ast.CompoundDatum}
 * @struct
 * @constructor
 */
r5js.ast.List = function(firstChild) {
  r5js.ast.List.base(this, 'constructor');
  if (firstChild) {
    this.setFirstChild(firstChild);
  }

  /** @private */ this.dirty_ = false;

  /** @private */ this.dotted_ = false;
};
goog.inherits(r5js.ast.List, r5js.ast.CompoundDatum);
r5js.IPair.addImplementation(r5js.ast.List);


/** Marks dirty. */
r5js.ast.List.prototype.markDirty = function() {
  this.dirty_ = true;
};


/** Marks dotted. */
r5js.ast.List.prototype.markDotted = function() {
  this.dotted_ = true;
};


/** @override */
r5js.ast.List.prototype.isImproperList = function() {
  return this.dotted_;
};


/** @override */
r5js.ast.List.prototype.eqv = function(other) {
  if (this.dotted_) {
    return this === other;
  }

  if (!(other instanceof r5js.ast.CompoundDatum)) {
    return false;
  }

  if (this === other ||
      (other instanceof r5js.ast.List &&
      !this.getFirstChild() &&
      !other.getFirstChild())) {
    return true;
  }

  const thisHelper = this.getCdrHelper();
  let otherHelper = other.getCdrHelper();
  if (thisHelper && otherHelper) {
    return thisHelper.equals(otherHelper);
  } else if (thisHelper && other instanceof r5js.ast.CompoundDatum) {
    return thisHelper.resolvesTo(other);
  } else if (otherHelper) {
    return otherHelper.resolvesTo(this);
  } else {
    return false;
  }
};


/** @override */
r5js.ast.List.prototype.car = function() {
  return /** @type {!r5js.Datum} */ (this.getFirstChild());
};


/**
 * @override
 * @suppress {checkTypes} for setNextSibling(null).
 */
r5js.ast.List.prototype.cdr = function() {
    const startOfCdr = this.getFirstChild().getNextSibling();
    if (!startOfCdr) {
        return new r5js.SiblingBuffer().toList(r5js.ast.List);
    }

    if (startOfCdr.getNextSibling() || !this.dirty_) {
        // TODO bl investigate why this is happening
        if (startOfCdr.getNextSibling() === startOfCdr) {
            startOfCdr.setNextSibling(null);
        }
        return new r5js.SiblingBuffer()
            .appendSibling(startOfCdr)
            .toList(this.dotted_ ? r5js.ast.DottedList : r5js.ast.List);
    } else {
        return startOfCdr;
    }
};
