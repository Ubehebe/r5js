goog.module('r5js.ast.UnquoteSplicing');

const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');

class UnquoteSplicing extends CompoundDatum {
    /** @param {!Datum} firstChild */
    constructor(firstChild) {
        super();
        if (firstChild) {
            this.setFirstChild(firstChild);
        }
    }

    /** @override */
    setQuasiquotationLevel(qqLevel) {
        this.qqLevel = qqLevel;
        return super.setQuasiquotationLevel(qqLevel - 1);
    }
}

exports = UnquoteSplicing;
