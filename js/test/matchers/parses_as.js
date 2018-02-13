goog.module('parseAs');

goog.setTestOnly('parseAs');

const Datum = goog.require('r5js.Datum');
const Matcher = goog.require('tdd.matchers.Matcher');
const ParserImpl = goog.require('r5js.ParserImpl');
const Reader = goog.require('r5js.Reader');
const TokenStream = goog.require('r5js.TokenStream');
const {Nonterminal} = require('/js/parse/nonterminals_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');

/** @implements {Matcher<string>} */
class ParsesAs {
    /** @param {!Nonterminal} expectedType */
    constructor(expectedType) {
        /** @const @private {!Nonterminal} */
        this.expectedType_ = expectedType;

        /** @private {!Nonterminal|null} */
        this.actualType_ = null;
    }

    /** @override */
    matches(value) {
        let datumRoot;
        try {
            datumRoot = Reader.forTokenStream(TokenStream.forText(value)).read();
        } catch (e) {
            return false;
        }
        const actualResult = (datumRoot instanceof Datum)
            && new ParserImpl.ParserImpl(datumRoot).parse(this.expectedType_);
        if (actualResult && actualResult.peekParse) {
            this.actualType_ = /** @type {!Nonterminal} */ (actualResult.peekParse());
        }
        return this.actualType_ === this.expectedType_;
    }

    /** @override */
    getFailureMessage(value) {
        return 'expected ' +
            value +
            ' to parse as ' +
            this.expectedType_ +
            ', got ' +
            this.actualType_;
    }
}

/**
 * @param {!Nonterminal} expectedType
 * @return {!Matcher}
 */
function parseAs(expectedType) {
    return new ParsesAs(expectedType);
}

exports = parseAs;

