goog.provide('r5js.InMemoryInputPort');
goog.provide('r5js.InMemoryOutputPort');

goog.require('r5js.InMemoryPortBuffer');
goog.require('r5js.InputPort');
goog.require('r5js.OutputPort');
goog.require('r5js.OutputSavingPort');
goog.require('r5js.ReaderImpl');
goog.require('r5js.Scanner');
goog.require('r5js.ast.Character');
goog.require('r5js.valutil');

/**
 * @param {!r5js.InMemoryPortBuffer} buffer
 * @implements {r5js.InputPort}
 * @struct
 * @constructor
 */
r5js.InMemoryInputPort = function(buffer) {
  /** @private */ this.closed_ = false;
  /** @const @private */ this.buffer_ = buffer;
  /** @private {r5js.Datum} */ this.leftoverDatum_ = null;
};
r5js.InputPort.addImplementation(r5js.InMemoryInputPort);


/** @override */
r5js.InMemoryInputPort.prototype.isCharReady = function() {
  return !this.buffer_.isEmpty();
};


/** @override */
r5js.InMemoryInputPort.prototype.close = function() {
  this.closed_ = true;
};


/** @override */
r5js.InMemoryInputPort.prototype.read = function() {
  const maybeDatum = this.readLeftoverDatum_();
  if (maybeDatum) {
    return maybeDatum;
  } else if (this.buffer_.isEmpty()) {
    return null;
  } else {
    const text = this.buffer_.getAndClear();
    this.leftoverDatum_ = new r5js.ReaderImpl(
        new r5js.Scanner(text)).read();
    return this.read();
  }
};


/**
 * @return {r5js.Datum}
 * @private
 */
r5js.InMemoryInputPort.prototype.readLeftoverDatum_ = function() {
  const retval = this.leftoverDatum_;
  if (retval) {
    this.leftoverDatum_ = this.leftoverDatum_.getNextSibling();
  }
  return retval;
};


/** @override */
r5js.InMemoryInputPort.prototype.peekChar = function() {
  const c = this.buffer_.peekChar();
  return c ? new r5js.ast.Character(c) : null;
};


/** @override */
r5js.InMemoryInputPort.prototype.readChar = function() {
  const c = this.buffer_.getChar();
  return c ? new r5js.ast.Character(c) : null;
};



/**
 * @param {!r5js.InMemoryPortBuffer} buffer
 * @implements {r5js.OutputSavingPort}
 * @struct
 * @constructor
 */
r5js.InMemoryOutputPort = function(buffer) {
  /** @const @private */ this.buffer_ = buffer;
  /** @const @private {!Array<string>} */ this.outputs_ = [];
};
r5js.OutputPort.addImplementation(r5js.InMemoryOutputPort);


/** @override */
r5js.InMemoryOutputPort.prototype.write = function(str) {
  this.buffer_.append(str);
  this.outputs_.push(str);
};


/** @override */
r5js.InMemoryOutputPort.prototype.dequeueOutput = function() {
  return this.outputs_.shift();
};


/** @override */
r5js.InMemoryOutputPort.prototype.close = goog.nullFunction;
