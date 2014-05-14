goog.provide('r5js.InMemoryInputPort');
goog.provide('r5js.InMemoryOutputPort');
goog.provide('r5js.InMemoryPortBuffer');


goog.require('r5js.EvalAdapter');
goog.require('r5js.InputPort');
goog.require('r5js.OutputPort');
goog.require('r5js.runtime.EOF');


/** @typedef {!Array.<!r5js.ValueAndExternalRepresentation_>} */
r5js.InMemoryPortBuffer;



/**
 * @param {!r5js.InMemoryPortBuffer} buffer
 * @implements {r5js.InputPort}
 * @struct
 * @constructor
 */
r5js.InMemoryInputPort = function(buffer) {
  /** @private */ this.closed_ = false;
  /** @const @private */ this.buffer_ = buffer;
};
r5js.InputPort.addImplementation(r5js.InMemoryInputPort);


/** @override */
r5js.InMemoryInputPort.prototype.isCharReady = function() {
  return false; // TODO bl
};


/** @override */
r5js.InMemoryInputPort.prototype.close = function() {
  this.closed_ = true;
};


/** @override */
r5js.InMemoryInputPort.prototype.read = function() {
  if (!this.buffer_.length) {
    return r5js.runtime.EOF;
  }
  return this.buffer_.shift().value;
};


/** @override */
r5js.InMemoryInputPort.prototype.peekChar = function() {
  if (!this.buffer_.length) {
    return r5js.runtime.EOF;
  }
  return this.buffer_[0].peekChar();
};


/** @override */
r5js.InMemoryInputPort.prototype.readChar = function() {
  if (!this.buffer_.length) {
    return r5js.runtime.EOF;
  }
  var result = this.buffer_[0].readChar();
  if (this.buffer_[0].done()) {
    this.buffer_.shift();
  }
  return result;
};



/**
 * @param {!r5js.InMemoryPortBuffer} buffer
 * @implements {r5js.OutputPort}
 * @struct
 * @constructor
 */
r5js.InMemoryOutputPort = function(buffer) {
  /** @const @private */ this.buffer_ = buffer;
};
r5js.OutputPort.addImplementation(r5js.InMemoryOutputPort);


/** @override */
r5js.InMemoryOutputPort.prototype.display =
    r5js.InMemoryOutputPort.prototype.write;


/** @override */
r5js.InMemoryOutputPort.prototype.write = function(value) {
  this.buffer_.push(new r5js.ValueAndExternalRepresentation_(value));
};


/** @override */
r5js.InMemoryOutputPort.prototype.close = goog.nullFunction;



/**
 * @param {!r5js.runtime.Value} value
 * @struct
 * @constructor
 * @private
 */
r5js.ValueAndExternalRepresentation_ = function(value) {
  /** @const */
  this.value = value;
  /** @const @private */
  this.externalRepresentation_ = r5js.EvalAdapter.toWriteString(value);
  /** @private */
  this.externalRepresentationOffset_ = 0;
};


/** @return {boolean} */
r5js.ValueAndExternalRepresentation_.prototype.done = function() {
  return this.externalRepresentationOffset_ >=
      this.externalRepresentation_.length;
};


/** @return {string} */
r5js.ValueAndExternalRepresentation_.prototype.peekChar = function() {
  return this.externalRepresentation_[this.externalRepresentationOffset_];
};


/** @return {string} */
r5js.ValueAndExternalRepresentation_.prototype.readChar = function() {
  var result = this.peekChar();
  ++this.externalRepresentationOffset_;
  return result;
};
