goog.provide('r5js.test.Scanner');


goog.require('r5js.scan.TokenType');
goog.require('r5js.Scanner');
goog.require('r5js.test.fixtures');
goog.require('tdd.SyncTestSuite');


/**
 * Scanning-related tests.
 * @extends {tdd.SyncTestSuite}
 * @constructor
 */
r5js.test.Scanner = function() {
    goog.base(this, "r5js.test.Scanner");
};
goog.inherits(r5js.test.Scanner, tdd.SyncTestSuite);



/** @override */
r5js.test.Scanner.prototype.runTests = function() {
    r5js.scan.TokenType.ALL_TOKEN_TYPES.forEach(
        this.checkValidTokens_, this);
    r5js.scan.TokenType.ALL_TOKEN_TYPES.forEach(
        this.checkInvalidTokens_, this);
};


/**
 * @param {!r5js.scan.TokenType} tokenType The token type.
 * @private
 */
r5js.test.Scanner.prototype.checkValidTokens_ = function(tokenType) {
    r5js.test.fixtures.validTokensForType(tokenType).forEach(
        // PhantomJS doesn't have Function.prototype.bind yet.
        goog.bind(this.checkTokenOfType_, this, tokenType, true)
    );
};


/**
 * @param {!r5js.scan.TokenType} tokenType The token type.
 * @private
 */
r5js.test.Scanner.prototype.checkInvalidTokens_ = function(tokenType) {
    r5js.test.fixtures.invalidTokensForType(tokenType).forEach(
        // PhantomJS doesn't have Function.prototype.bind yet.
        goog.bind(this.checkTokenOfType_, this, tokenType, false)
    );
};


/**
 * @param {!r5js.scan.TokenType} tokenType
 * @param {boolean} expectedValid True iff the token is expected to be valid.
 * @param {string} token
 * @private
 */
r5js.test.Scanner.prototype.checkTokenOfType_ = function(
    tokenType, expectedValid, token) {
    var desc = expectedValid ? ' is a valid ' : ' is an invalid ';
    var actualValid = true;
    this.Do(
        token + desc + r5js.scan.tokenTypeName(tokenType),
        function() {
            try {
                new r5js.Scanner(token).tokenize();
            } catch (e) {
                actualValid = false;
            }
        }).expecting(actualValid).toBe(expectedValid);
};