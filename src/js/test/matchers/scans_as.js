goog.provide('scanAs');
goog.setTestOnly('scanAs');


goog.require('r5js.Scanner');


/**
 * @param {r5js.scan.TokenType} expectedTokenType
 * @return {!tdd.matchers.Matcher}
 */
scanAs = function(expectedTokenType) {
  return new r5js.test.matchers.ScansAs_(expectedTokenType);
};



/**
 * @param {!r5js.scan.TokenType} expectedTokenType
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.ScansAs_ = function(expectedTokenType) {
  /** @const @private {!r5js.scan.TokenType} */
  this.expectedTokenType_ = expectedTokenType;
};


/** @override */
r5js.test.matchers.ScansAs_.prototype.matches = function(value) {
  var actualValid = true;
  try {
    new r5js.Scanner(/** @type {string} */ (value)).tokenize();
  } catch (e) {
    actualValid = false;
  }
  return actualValid;
};


/** @override */
r5js.test.matchers.ScansAs_.prototype.getSuccessMessage = function(value) {
  return value + ' correctly scans as ' + this.expectedTokenType_;
};


/** @override */
r5js.test.matchers.ScansAs_.prototype.getFailureMessage = function(value) {
  return 'expected ' + value + ' to scan as ' + this.expectedTokenType_;
};
