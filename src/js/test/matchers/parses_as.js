goog.provide('parseAs');
goog.setTestOnly('parseAs');
goog.provide('r5js.test.matchers.ParsesAs');
goog.setTestOnly('r5js.test.matchers.ParsesAs');


goog.require('r5js.Datum');
goog.require('r5js.Parser');
goog.require('r5js.Reader');
goog.require('r5js.Scanner');


/**
 * @param {!r5js.parse.Nonterminal} expectedType
 * @return {!tdd.matchers.Matcher}
 */
parseAs = function(expectedType) {
  return new r5js.test.matchers.ParsesAs(expectedType);
};



/**
 * @param {!r5js.parse.Nonterminal} expectedType
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 */
r5js.test.matchers.ParsesAs = function(expectedType) {
  /** @const @private {!r5js.parse.Nonterminal} */
  this.expectedType_ = expectedType;
};


/** @override */
r5js.test.matchers.ParsesAs.prototype.matches = function(value) {
  var datumRoot = new r5js.Reader(
          new r5js.Scanner(/** @type {string} */ (value))).read();
  var actualResult = (datumRoot instanceof r5js.Datum) &&
      new r5js.Parser(datumRoot).rhs({type: this.expectedType_});
  return actualResult &&
      actualResult.peekParse &&
      actualResult.peekParse() === this.expectedType_;
};


/** @override */
r5js.test.matchers.ParsesAs.prototype.getSuccessMessage = function(value) {
  return value + ' correctly parses as ' + this.expectedType_;
};


/** @override */
r5js.test.matchers.ParsesAs.prototype.getFailureMessage = function(value) {
  return 'expected ' + value + ' to parse as ' + this.expectedType_;
};
