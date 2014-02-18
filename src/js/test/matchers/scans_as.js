goog.provide('scanAs');
goog.setTestOnly('scanAs');


goog.require('r5js.scan.tokenTypeForDatumType');
goog.require('r5js.scan.tokenTypeName');
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
  try {
    var scanner = new r5js.Scanner(/** @type {string} */ (value));
    var token = scanner.nextToken();
    // There should be exactly one token in the input.
    // (For example, 1+2 should fail to scan as one number token,
    // even though the whole input scans.)
    if (!token || scanner.nextToken()) {
      return false;
    }
    var asDatum = token.formatDatum(new r5js.Datum());
    return r5js.scan.tokenTypeForDatumType(
        /** @type {!r5js.DatumType} */ (asDatum.type)) ===
        this.expectedTokenType_;
  } catch (e) {
    return false; // some tests purposely cause scan errors
  }
};


/** @override */
r5js.test.matchers.ScansAs_.prototype.getSuccessMessage = function(value) {
  return value +
      ' correctly scans as ' +
      r5js.scan.tokenTypeName(this.expectedTokenType_);
};


/** @override */
r5js.test.matchers.ScansAs_.prototype.getFailureMessage = function(value) {
  return 'expected ' +
      value +
      ' to scan as ' +
      r5js.scan.tokenTypeName(this.expectedTokenType_);
};
