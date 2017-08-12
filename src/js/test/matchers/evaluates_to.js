goog.module('evalTo');
goog.module.declareLegacyNamespace();
goog.setTestOnly('evalTo');

const Matcher = goog.require('tdd.matchers.Matcher');

/** @implements {Matcher<string>} */
class EvaluatesTo {
    /** @param {string} expectedValue */
    constructor(expectedValue) {
        /** @const @private */ this.expectedValue_ = expectedValue;
    }

    /** @override */
    matches(actualValue) {
        return this.expectedValue_ === actualValue;
    }

    /** @override */
    getFailureMessage(actualValue) {
        return 'want ' +
            this.expectedValue_ +
            ' got ' +
            actualValue;
    }
}

/**
 * @param {string} value
 * @return {!Matcher<string>}
 */
function evalTo(value) {
    return new EvaluatesTo(value);
}

exports = evalTo;

