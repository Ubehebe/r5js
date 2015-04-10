goog.module('r5js.ast.DottedList');

const CdrHelperImpl = goog.require('r5js.CdrHelperImpl');
const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const IPair = goog.require('r5js.IPair');
const List = goog.require('r5js.ast.List');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');

/** @implements {IPair} */
class DottedList extends CompoundDatum {
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

IPair.addImplementation(DottedList);

exports = DottedList;

