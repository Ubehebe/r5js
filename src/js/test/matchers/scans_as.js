goog.module('scanAs');
goog.setTestOnly('scanAs');

const Datum = goog.require('r5js.Datum');
const Matcher = goog.require('tdd.matchers.Matcher');
const TokenStream = goog.require('r5js.TokenStream');

/** @implements {Matcher<string>} */
class ScansAs {
    /** @param {function(new: Datum, ?)} expectedType */
    constructor(expectedType) {
        /** @const @private */ this.expectedType_ = expectedType;
    }

    /** @override */
    matches(value) {
        try {
            const scanner = TokenStream.forText(value);
            const token = scanner.nextToken();
            // There should be exactly one token in the input.
            // (For example, 1+2 should fail to scan as one number token,
            // even though the whole input scans.)
            if (!token || scanner.nextToken()) {
                return false;
            }
            return token instanceof this.expectedType_;
        } catch (e) {
            return false; // some tests purposely cause scan errors
        }
    }

    /** @override */
    getFailureMessage(value) {
        return 'expected ' + value + ' to scan as ' + this.expectedType_;
    }
}

/**
 * @param {function(new: Datum, ?)} expectedType
 * @return {!Matcher<string>}
 */
function scanAs(expectedType) {
    return new ScansAs(expectedType);
}

exports = scanAs;
