goog.provide('r5js.SchemeSources');

goog.require('PROCEDURES');
goog.require('SYNTAX');
goog.require('goog.Promise');

r5js.SchemeSources = class {
    /**
     * @param {string} syntax
     * @param {string} procedures
     */
    constructor(syntax, procedures) {
        /** @const */ this.syntax = syntax;
        /** @const */ this.procedures = procedures;
    }

    /** @return {!goog.Promise<!r5js.SchemeSources>} */
    static get() {
        return goog.Promise.resolve(new r5js.SchemeSources(SYNTAX, PROCEDURES));
    }
};
