goog.provide('r5js.scan.TokenStreamImpl');



/**
 * @param {!r5js.Scanner} scanner
 * @implements {r5js.scan.TokenStream}
 * @struct
 * @constructor
 */
r5js.scan.TokenStreamImpl = function(scanner) {
  /** @const @private {!r5js.Scanner} */
  this.tokenStream_ = scanner;

  /** @const @private {!Array.<!r5js.Token>} */
  this.readyTokens_ = [];

  /** @private {number} */
  this.nextTokenIndex_ = 0;
};


/** @override */
r5js.scan.TokenStreamImpl.prototype.checkpoint = function() {
  return this.nextTokenIndex_;
};


/** @override */
r5js.scan.TokenStreamImpl.prototype.nextToken = function() {
  while (this.nextTokenIndex_ >= this.readyTokens_.length) {
    var token = this.tokenStream_.nextToken();
    if (!token) {
      return null;
    }
    this.readyTokens_.push(token);
  }
  return this.readyTokens_[this.nextTokenIndex_++];
};


/** @override */
r5js.scan.TokenStreamImpl.prototype.restore = function(checkpoint) {
  this.nextTokenIndex_ = checkpoint;
};
