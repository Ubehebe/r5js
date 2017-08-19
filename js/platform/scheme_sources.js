goog.module('r5js.SchemeSources');
goog.module.declareLegacyNamespace();

const procedures = goog.require('PROCEDURES');
const syntax = goog.require('SYNTAX');

class SchemeSources {
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
