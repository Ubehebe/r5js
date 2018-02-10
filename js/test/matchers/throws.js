goog.module('Throw');

goog.setTestOnly('Throw');

const Matcher = goog.require('tdd.matchers.Matcher');
const {Error} = goog.require('r5js.Error');

/** @implements {Matcher<!Error>} */
class Throws {
    /** @param {!Error} expectedError */
    constructor(expectedError) {
        /** @const @private */ this.expectedError_ = expectedError;
        /** @private */ this.actualError_ = null;
    }

    /** @override */
    matches(actualError) {
        return this.expectedError_.equals(this.actualError_ = actualError);
    }

    /** @override */
    getFailureMessage(input) {
        return input +
            ': want\n' +
            this.expectedError_.toString() +
            '\ngot ' +
            (this.actualError_ ?
                ('\n' + this.actualError_.toString()) :
                'no exception');
    }
}

/**
 * @param {!Error} error
 * @return {!Matcher<!Error>}
 */
function Throw(error) {
    return new Throws(error);
}

exports = Throw;

