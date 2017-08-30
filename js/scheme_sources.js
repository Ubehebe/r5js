goog.module('r5js.SchemeSources');

const procedures = goog.require('PROCEDURES');
const syntax = goog.require('SYNTAX');

class SchemeSources {
    constructor() {
        /** @const */ this.syntax = syntax;
        /** @const */ this.procedures = procedures;
    }
}

exports = SchemeSources;
