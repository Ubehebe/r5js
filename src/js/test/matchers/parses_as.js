goog.provide('r5js.test.matchers.ParsesAs');
goog.setTestOnly('r5js.test.matchers.ParsesAs');


goog.require('r5js.Datum');
goog.require('r5js.Parser');
goog.require('r5js.Reader');
goog.require('r5js.Scanner');


/**
 * @param {string} expectedType
 * @param {string} text
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 */
r5js.test.matchers.ParsesAs = function(expectedType, text) {
    /** @const @private {string} */
    this.expectedType_ = expectedType;

    /** @const @private {string} */
    this.text_ = text;
};


/** @override */
r5js.test.matchers.ParsesAs.prototype.matches = function() {
    var datumRoot = new r5js.Reader(new r5js.Scanner(this.text_)).read();
    var actualResult = (datumRoot instanceof r5js.Datum) &&
        new r5js.Parser(datumRoot).rhs({type: this.expectedType_});
    return actualResult &&
        actualResult.peekParse &&
        actualResult.peekParse() === this.expectedType_;
};


/** @override */
r5js.test.matchers.ParsesAs.prototype.getSuccessMessage = function() {
    return this.text_ + ' correctly parses as ' + this.expectedType_;
};


/** @override */
r5js.test.matchers.ParsesAs.prototype.getFailureMessage = function() {
    return 'expected ' + this.text_ + ' to parse as ' + this.expectedType_;
};