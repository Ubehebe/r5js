goog.module('r5js.ast.Unquote');

const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');

class Unquote extends CompoundDatum {
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

exports = Unquote;
