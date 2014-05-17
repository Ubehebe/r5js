goog.provide('r5js.InMemoryInputPort');
goog.provide('r5js.InMemoryOutputPort');
goog.provide('r5js.InMemoryPortBuffer');


goog.require('r5js.EvalAdapter');
goog.require('r5js.IOError');
goog.require('r5js.InputPort');
goog.require('r5js.OutputPort');
goog.require('r5js.ParserImpl');
goog.require('r5js.Reader');
goog.require('r5js.Scanner');


/** @typedef {!Array.<!r5js.BufferedValue_>} */
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
  return this.buffer_.length > 0 && !!this.buffer_[0].peekChar();
};


/** @override */
r5js.InMemoryInputPort.prototype.close = function() {
  this.closed_ = true;
};


/** @override */
r5js.InMemoryInputPort.prototype.read = function() {
  return this.buffer_.length ? this.buffer_.shift().toDatum() : null;
};


/** @override */
r5js.InMemoryInputPort.prototype.peekChar = function() {
  var c;
  if (!this.buffer_.length || !(c = this.buffer_[0].peekChar())) {
    return null;
  }
  return new r5js.ast.Character(c);
};


/** @override */
r5js.InMemoryInputPort.prototype.readChar = function() {
  var c;
  if (!this.buffer_.length || !(c = this.buffer_[0].readChar())) {
    return null;
  }
  if (!this.buffer_[0].peekChar()) {
    this.buffer_.shift();
  }
  return new r5js.ast.Character(c);
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
r5js.InMemoryOutputPort.prototype.write = function(value) {
  this.buffer_.push(new r5js.ValueAndExternalRepresentation_(value));
};


/** @override */
r5js.InMemoryOutputPort.prototype.writeChar = function(c) {
  this.getValueUnderConstruction_().push(c);
};


/**
 * @return {!r5js.ValueUnderConstruction_}
 * @private
 */
r5js.InMemoryOutputPort.prototype.getValueUnderConstruction_ = function() {
  if (!this.buffer_.length ||
      !(this.buffer_[this.buffer_.length - 1] instanceof
          r5js.ValueUnderConstruction_)) {
    this.buffer_.push(new r5js.ValueUnderConstruction_());
  }
  return /** @type {!r5js.ValueUnderConstruction_} */ (
      this.buffer_[this.buffer_.length - 1]);
};


/** @override */
r5js.InMemoryOutputPort.prototype.display =
    r5js.InMemoryOutputPort.prototype.write;


/** @override */
r5js.InMemoryOutputPort.prototype.close = goog.nullFunction;



/**
 * @interface
 * @private
 */
r5js.BufferedValue_ = function() {};


/** @return {?string} */
r5js.BufferedValue_.prototype.peekChar = function() {};


/** @return {?string} */
r5js.BufferedValue_.prototype.readChar = function() {};


/** @return {!r5js.Datum} */
r5js.BufferedValue_.prototype.toDatum = function() {};



/**
 * @param {!r5js.runtime.Value} value
 * @implements {r5js.BufferedValue_}
 * @struct
 * @constructor
 * @private
 */
r5js.ValueAndExternalRepresentation_ = function(value) {
  /** @const @private */
  this.value_ = value;
  /** @const @private */
  this.externalRepresentation_ = r5js.EvalAdapter.toWriteString(value);
  /** @private */
  this.pos_ = 0;
};


/** @override */
r5js.ValueAndExternalRepresentation_.prototype.peekChar = function() {
  return this.pos_ < this.externalRepresentation_.length ?
      this.externalRepresentation_[this.pos_] : null;
};


/** @override */
r5js.ValueAndExternalRepresentation_.prototype.readChar = function() {
  var result = this.peekChar();
  if (result) {
    ++this.pos_;
  }
  return result;
};


/**
 * @override
 * @suppress {checkTypes}
 */
r5js.ValueAndExternalRepresentation_.prototype.toDatum = function() {
  return this.value_;
};



/**
 * @implements {r5js.BufferedValue_}
 * @struct
 * @constructor
 * @private
 */
r5js.ValueUnderConstruction_ = function() {
  /** @const @private {!Array.<string>} */ this.buffer_ = [];
};


/** @param {string} c*/
r5js.ValueUnderConstruction_.prototype.push = function(c) {
  this.buffer_.push(c);
};


/** @override */
r5js.ValueUnderConstruction_.prototype.peekChar = function() {
  return this.buffer_.length ? this.buffer_[0] : null;
};


/** @override */
r5js.ValueUnderConstruction_.prototype.readChar = function() {
  return this.buffer_.length ? this.buffer_.shift() : null;
};


/** @override */
r5js.ValueUnderConstruction_.prototype.toDatum = function() {
  var text = this.buffer_.join('');
  var ans = new r5js.ParserImpl(
      new r5js.Reader(
      new r5js.Scanner(
      text)).read()).parse();
  if (ans) {
    return ans;
  } else {
    throw new r5js.IOError('read failure: ' + text);
  }
};
