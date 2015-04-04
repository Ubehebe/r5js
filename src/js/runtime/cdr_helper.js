goog.module('r5js.CdrHelper');

const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const List = goog.require('r5js.ast.List');
const error = goog.require('r5js.error');

/**
 * See the comment to {@link r5js.Datum.siblingsToList}
 * for an explanation of what this class does.
 */
class CdrHelper {
    /**
     * @param {!CompoundDatum} head
     * @param {!Datum} startOfCdr
     */
    constructor(head, startOfCdr) {
        /** @const @private */ this.head_ = head;

        /** @const @private {!Datum} */
        this.startOfCdr_ = startOfCdr;
    }

    /**
     * Basically, call set-car! on the master list.
     * @param {!Datum} car TODO bl.
     */
    setCar(car) {
        if (this.head_.isImmutable()) {
            throw error.immutable(this.head_.toString());
        }
        this.head_.getFirstChild().setNextSibling(car);
    }

    /**
     * Basically, call set-cdr! on the master list.
     * @param {!Datum} cdr TODO bl.
     */
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

    /**
     * @param {!CdrHelper} cdrHelper Helper to compare against.
     * @return {boolean} True iff the two CdrHelpers point to the same list
     * and have the same offset.
     */
    equals(cdrHelper) {
        return this.head_ === cdrHelper.head_ &&
            this.startOfCdr_ === cdrHelper.startOfCdr_;
    }

    /**
     * @param {!CompoundDatum} datum The datum to test against.
     * @return {boolean} True iff the CdrHelper points to the given list datum
     * and its offset is that list's first child.
     */
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

exports = CdrHelper;