goog.module('r5js.SchemeSources');

const procedures = goog.require('PROCEDURES');
const syntax = goog.require('SYNTAX');
const promise = goog.require('goog.Promise');

class SchemeSources {
    constructor() {
        /** @const */ this.syntax = syntax;
        /** @const */ this.procedures = procedures;
    }

    /** @return {!goog.Promise<!r5js.SchemeSources>} */
    static get() {
        return promise.resolve(new r5js.SchemeSources());
    }
}

exports = SchemeSources;
