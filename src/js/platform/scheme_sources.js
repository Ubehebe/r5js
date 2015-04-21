goog.module('r5js.SchemeSources');

const procedures = goog.require('PROCEDURES');
const syntax = goog.require('SYNTAX');

class SchemeSources {
    /** @private */
    constructor() {
        /** @const */ this.syntax = syntax;
        /** @const */ this.procedures = procedures;
    }

    /** @return {!SchemeSources} */
    static get() {
        return new SchemeSources();
    }
}

exports = SchemeSources;
