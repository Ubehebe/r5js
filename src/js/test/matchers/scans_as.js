goog.provide('scanAs');
goog.setTestOnly('scanAs');


goog.require('r5js.Scanner');


/**
 * @param {!r5js.DatumType} expectedType
 * @return {!tdd.matchers.Matcher}
 */
scanAs = function(expectedType) {
  return new r5js.test.matchers.ScansAs_(expectedType);
};



/**
 * @param {!r5js.DatumType} expectedType
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.ScansAs_ = function(expectedType) {
  /** @const @private {!r5js.DatumType} */
  this.expectedType_ = expectedType;
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
    return asDatum.getType() === this.expectedType_;
  } catch (e) {
    return false; // some tests purposely cause scan errors
  }
};


/** @override */
r5js.test.matchers.ScansAs_.prototype.getSuccessMessage = function(value) {
  return value + ' correctly scans as ' + this.expectedType_;
};


/** @override */
r5js.test.matchers.ScansAs_.prototype.getFailureMessage = function(value) {
  return 'expected ' + value + ' to scan as ' + this.expectedType_;
};
