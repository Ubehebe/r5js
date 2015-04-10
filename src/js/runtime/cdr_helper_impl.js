goog.module('r5js.CdrHelperImpl');

const CdrHelper = goog.require('r5js.CdrHelper');
const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const List = goog.require('r5js.ast.List');
const error = goog.require('r5js.error');

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
            throw error.immutable(this.head_.toString());
        }
        this.head_.getFirstChild().setNextSibling(car);
    }

    /** @override */
    setCdr(cdr) {
        if (this.head_.isImmutable()) {
            throw error.immutable(this.head_.toString());
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
}

exports = CdrHelperImpl;