/* Copyright 2011-2014 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

goog.provide('r5js.InMemoryInputPort');
goog.provide('r5js.InMemoryOutputPort');
goog.provide('r5js.InMemoryPortBuffer');


goog.require('r5js.InputPort');
goog.require('r5js.OutputPort');
goog.require('r5js.ReaderImpl');
goog.require('r5js.Scanner');
goog.require('r5js.ast.Character');
goog.require('r5js.valutil');



/**
 * @struct
 * @constructor
 */
r5js.InMemoryPortBuffer = function() {
  /** @private */ this.buffer_ = '';
};


/** @return {boolean} */
r5js.InMemoryPortBuffer.prototype.isEmpty = function() {
  return !this.buffer_.length;
};


/** @return {?string} */
r5js.InMemoryPortBuffer.prototype.getChar = function() {
  var c = this.peekChar();
  if (c) {
    this.buffer_ = this.buffer_.substr(1);
  }
  return c;
};


/** @return {string?} */
r5js.InMemoryPortBuffer.prototype.peekChar = function() {
  return this.buffer_.length ? this.buffer_.charAt(0) : null;
};


/** @return {string} */
r5js.InMemoryPortBuffer.prototype.getAndClear = function() {
  var retval = this.buffer_;
  this.buffer_ = '';
  return retval;
};


/** @param {string} str */
r5js.InMemoryPortBuffer.prototype.append = function(str) {
  this.buffer_ += str;
};



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
  var maybeDatum = this.readLeftoverDatum_();
  if (maybeDatum) {
    return maybeDatum;
  } else if (this.buffer_.isEmpty()) {
    return null;
  } else {
    var text = this.buffer_.getAndClear();
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
  var retval = this.leftoverDatum_;
  if (retval) {
    this.leftoverDatum_ = this.leftoverDatum_.getNextSibling();
  }
  return retval;
};


/** @override */
r5js.InMemoryInputPort.prototype.peekChar = function() {
  var c = this.buffer_.peekChar();
  return c ? new r5js.ast.Character(c) : null;
};


/** @override */
r5js.InMemoryInputPort.prototype.readChar = function() {
  var c = this.buffer_.getChar();
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
