goog.provide('r5js.Pipe');


goog.require('r5js.EvalAdapter');
goog.require('r5js.InputPort');
goog.require('r5js.OutputPort');
goog.require('r5js.runtime.EOF');



/**
 * @implements {r5js.InputPort}
 * @implements {r5js.OutputPort}
 * @struct
 * @constructor
 */
r5js.Pipe = function() {
  /** @private */ this.closed_ = false;

  /**
     * @private {!Array.<!r5js.Pipe.ValueAndExternalRepresentation_>}
     * @const
     */
  this.awaitingRead_ = [];
};
r5js.InputPort.addImplementation(r5js.Pipe);
r5js.OutputPort.addImplementation(r5js.Pipe);


/** @override */
r5js.Pipe.prototype.isCharReady = function() {
  return false; // TODO bl
};


/** @override */
r5js.Pipe.prototype.close = function() {
  this.closed_ = true;
};


/** @override */
r5js.Pipe.prototype.display = r5js.Pipe.prototype.write;


/** @override */
r5js.Pipe.prototype.write = function(value) {
  this.awaitingRead_.push(
      new r5js.Pipe.ValueAndExternalRepresentation_(value));
};


/** @override */
r5js.Pipe.prototype.read = function() {
  if (!this.awaitingRead_.length) {
    return r5js.runtime.EOF;
  }
  return this.awaitingRead_.shift().value;
};


/** @override */
r5js.Pipe.prototype.peekChar = function() {
  if (!this.awaitingRead_.length) {
    return r5js.runtime.EOF;
  }
  return this.awaitingRead_[0].peekChar();
};


/** @override */
r5js.Pipe.prototype.readChar = function() {
  if (!this.awaitingRead_.length) {
    return r5js.runtime.EOF;
  }
  var result = this.awaitingRead_[0].readChar();
  if (this.awaitingRead_[0].done()) {
    this.awaitingRead_.shift();
  }
  return result;
};



/**
 * @param {!r5js.runtime.Value} value
 * @struct
 * @constructor
 * @private
 */
r5js.Pipe.ValueAndExternalRepresentation_ = function(value) {
  /** @const */
  this.value = value;
  /** @const @private */
  this.externalRepresentation_ = r5js.EvalAdapter.toWriteString(value);
  /** @private */
  this.externalRepresentationOffset_ = 0;
};


/** @return {boolean} */
r5js.Pipe.ValueAndExternalRepresentation_.prototype.done = function() {
  return this.externalRepresentationOffset_ >=
      this.externalRepresentation_.length;
};


/** @return {string} */
r5js.Pipe.ValueAndExternalRepresentation_.prototype.peekChar =
    function() {
  return this.externalRepresentation_[this.externalRepresentationOffset_];
};


/** @return {string} */
r5js.Pipe.ValueAndExternalRepresentation_.prototype.readChar =
    function() {
  var result = this.peekChar();
  ++this.externalRepresentationOffset_;
  return result;
};
