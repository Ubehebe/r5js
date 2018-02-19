goog.module('r5js.ast.List');

const CdrHelper = goog.require('r5js.ast.CdrHelper');
const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const {Datum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const {IPair, addImplementation: addPairImpl} = require('/js/ast/ipair_collect_es6_sources.es6/node_modules/__main__/js/ast/ipair');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');

class List extends CompoundDatum /* implicitly implements IPair (structural interface) */ {
    /** @param {?Datum} firstChild */
    constructor(firstChild) {
        super();
        if (firstChild) {
            this.setFirstChild(firstChild);
        }
        /** @private */ this.dirty_ = false;
        /** @private */ this.dotted_ = false;
    }

    /** Marks dirty. */
    markDirty() {
        this.dirty_ = true;
    }

    /** Marks dotted. */
    markDotted() {
        this.dotted_ = true;
    }

    /** @override */
    isImproperList() {
        return this.dotted_;
    }

    /** @override */
    eqv(other) {
        if (this.dotted_) {
            return this === other;
        }

        if (!(other instanceof CompoundDatum)) {
            return false;
        }

        if (this === other
            || (other instanceof List
            && !this.getFirstChild()
            && !other.getFirstChild())) {
            return true;
        }

        const thisHelper = this.getCdrHelper();
        let otherHelper = other.getCdrHelper();
        if (thisHelper && otherHelper) {
            return thisHelper.equals(otherHelper);
        } else if (thisHelper && other instanceof CompoundDatum) {
            return thisHelper.resolvesTo(other);
        } else if (otherHelper) {
            return otherHelper.resolvesTo(this);
        } else {
            return false;
        }
    }

    /** @return {!Value} */
    car() {
        return /** @type {!Datum} */ (this.getFirstChild());
    }

    /** @return {!Value} */
    cdr() {
        const startOfCdr = this.getFirstChild().getNextSibling();
        if (!startOfCdr) {
            return new SiblingBuffer().toList(List);
        }

        if (startOfCdr.getNextSibling() || !this.dirty_) {
            // TODO bl investigate why this is happening
            if (startOfCdr.getNextSibling() === startOfCdr) {
                startOfCdr.setNextSibling(null);
            }
            return new SiblingBuffer()
                .appendSibling(startOfCdr)
                .toList(this.dotted_ ? DottedList : List);
        } else {
            return startOfCdr;
        }
    }
}
addPairImpl(List);

class DottedList extends CompoundDatum /* implicitly implements IPair (structural interface) */ {
    /** @param {?Datum} firstChild */
    constructor(firstChild) {
        super();
        if (firstChild) {
            this.setFirstChild(firstChild);
        }
    }

    /** @override */
    isImproperList() {
        return true;
    }

    /** @return {!Value} */
    car() {
        return /** @type {!Datum} */ (this.getFirstChild());
    }

    /** @return {!Value} */
    cdr() {
        const startOfCdr = this.getFirstChild().getNextSibling();
        let ans;
        if (startOfCdr) {
            if (startOfCdr.getNextSibling()) {
                ans = new SiblingBuffer()
                    .appendSibling(startOfCdr)
                    .toList(DottedList);
            } else {
                ans = startOfCdr;
            }
            if (ans instanceof CompoundDatum) {
                ans.setCdrHelper(new CdrHelperImpl(this, startOfCdr));
            }
            return ans;
        } else {
            return new SiblingBuffer().toList(List);
        }
    }
}
addPairImpl(DottedList);

/** @implements {CdrHelper} */
class CdrHelperImpl {
    /**
     * @param {!CompoundDatum} head
     * @param {!Datum} startOfCdr
     */
    constructor(head, startOfCdr) {
        /** @const @private */ this.head_ = head;

        /** @const @private {!Datum} */
        this.startOfCdr_ = startOfCdr;
    }

    /** @override */
    setCar(car) {
        if (this.head_.isImmutable()) {
            throw Error.immutable(this.head_.toString());
        }
        this.head_.getFirstChild().setNextSibling(car);
    }

    /** @override */
    setCdr(cdr) {
        if (this.head_.isImmutable()) {
            throw Error.immutable(this.head_.toString());
        }
        this.startOfCdr_.setNextSibling(cdr);
        if (!(cdr instanceof List)) {
            let cur = this;
            do {
                if (cur.head_ instanceof List) {
                    cur.head_.markDotted();
                }
            } while (cur = cur.head_.getCdrHelper());
        }
    }

    /** @override */
    equals(cdrHelper) {
        const asImpl = /** @type {!CdrHelperImpl} */ (cdrHelper);
        return this.head_ === asImpl.head_ &&
            this.startOfCdr_ === asImpl.startOfCdr_;
    }

    /** @override */
    resolvesTo(datum) {
        if (!datum) {
            return false;
        } else if (this.head_ === datum) {
            return this.startOfCdr_ === datum.getFirstChild();
        } else {
            return false;
        }
    }
}

exports = {List, CdrHelperImpl, DottedList};
