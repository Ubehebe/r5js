goog.module('r5js.ast.UnquoteSplicing');

const {CompoundDatum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/compound_datum');
const {Datum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');

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
