goog.module('r5js.SchemeSources');

const procedures = goog.require('PROCEDURES');
const syntax = goog.require('SYNTAX');
const Promise = goog.require('goog.Promise');

class SchemeSources {
    constructor() {
        /** @const */ this.syntax = syntax;
        /** @const */ this.procedures = procedures;
    }

    /** @return {!Promise<!SchemeSources>} */
    static get() {
        return Promise.resolve(new SchemeSources());
    }
}

exports = SchemeSources;
