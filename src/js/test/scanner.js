goog.provide('r5js.test.Scanner');


goog.require('expect');
goog.require('r5js.Scanner');
goog.require('r5js.scan.TokenType');
goog.require('r5js.scan.tokenTypeName');
goog.require('r5js.test.fixtures');
goog.require('tdd.TestType');



/**
 * Scanning-related tests.
 * @implements {tdd.TestSuite}
 * @struct
 * @constructor
 */
r5js.test.Scanner = function() {};


/** @override */
r5js.test.Scanner.prototype.getType = function() {
  return tdd.TestType.UNIT;
};


/** @override */
r5js.test.Scanner.prototype.toString = function() {
  return 'r5js.test.Scanner';
};


r5js.test.Scanner.prototype['testValidTokens'] = function() {
  r5js.scan.TokenType.ALL_TOKEN_TYPES.forEach(
      this.checkValidTokens_, this);
};


r5js.test.Scanner.prototype['testInvalidTokens'] = function() {
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
  var scans = true;
  var tokens = [];
  try {
    tokens = new r5js.Scanner(token).tokenize();
  } catch (e) {
    // Exceptions are expected for invalid tokens
  }
  if (expectedValid) {
    expect(tokens.length).toBe(1);
    expect(tokens[0].type).toBe(r5js.scan.tokenTypeName(tokenType));
  } else {
    expect(tokens.length).toBe(0);
  }
};
