goog.module('r5js.test.SchemeSources');
goog.setTestOnly('r5js.test.SchemeSources');

const negativeTests = goog.require('NEGATIVE_TESTS');
const otherTests = goog.require('OTHER_TESTS');
const r5RSTests = goog.require('R5RS_TESTS');
const testFramework = goog.require('TEST_FRAMEWORK');
const testFrameworkTests = goog.require('TEST_FRAMEWORK_TESTS');

class SchemeSources {
    /** @private */
    constructor() {
        /** @const */ this.testFramework = testFramework;
        /** @const */ this.testFrameworkTests = testFrameworkTests;
        /** @const */ this.r5RSTests = r5RSTests;
        /** @const */ this.negativeTests = negativeTests;
        /** @const */ this.otherTests = otherTests;
    }

    /** @return {!SchemeSources} */
    static get() {
            return new SchemeSources();
    }
}

exports = SchemeSources;
