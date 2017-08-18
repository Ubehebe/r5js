goog.module('r5js.ast.List');

const CdrHelper = goog.require('r5js.CdrHelper');
const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const Error = goog.require('r5js.Error');
const IPair = goog.require('r5js.IPair');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');

/** @implements {IPair} */
class List extends CompoundDatum {
    /** @param {Datum} firstChild */
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

    /** @override */
    car() {
        return /** @type {!Datum} */ (this.getFirstChild());
    }

    /**
     * @override
     * @suppress {checkTypes} for setNextSibling(null).
     */
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
                .toList(this.dotted_ ? List.Dotted : List);
        } else {
            return startOfCdr;
        }
    }
}
IPair.addImplementation(List);

List.Dotted = /** @implements {IPair} */ class extends CompoundDatum {
    /** @param {Datum} firstChild */
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

    /** @override */
    car() {
        return /** @type {!Datum} */ (this.getFirstChild());
    }

    /** @override */
    cdr() {
        const startOfCdr = this.getFirstChild().getNextSibling();
        let ans;
        if (startOfCdr) {
            if (startOfCdr.getNextSibling()) {
                ans = new SiblingBuffer()
                    .appendSibling(startOfCdr)
                    .toList(List.Dotted);
            } else {
                ans = startOfCdr;
            }
            if (ans instanceof CompoundDatum) {
                ans.setCdrHelper(new List.CdrHelperImpl(this, startOfCdr));
            }
            return ans;
        } else {
            return new SiblingBuffer().toList(List);
        }
    }
};
IPair.addImplementation(List.Dotted);

List.CdrHelperImpl = /** @implements {CdrHelper} */ class {
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
        return this.head_ === cdrHelper.head_ &&
            this.startOfCdr_ === cdrHelper.startOfCdr_;
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
};

exports = List;
