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


/** @typedef {!Array.<!r5js.JsonValue>} */
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
  return this.buffer_.length > 0;
};


/** @override */
r5js.InMemoryInputPort.prototype.close = function() {
  this.closed_ = true;
};


/** @override */
r5js.InMemoryInputPort.prototype.read = function() {
  if (!this.buffer_.length) {
    return null;
  }
  return new r5js.ReaderImpl(
      new r5js.Scanner(
          this.buffer_.shift().writeValue)).read();
};


/** @override */
r5js.InMemoryInputPort.prototype.peekChar = function() {
  return this.buffer_.length ?
      new r5js.ast.Character(this.buffer_[0].writeValue.charAt(0)) :
      null;
};


/** @override */
r5js.InMemoryInputPort.prototype.readChar = function() {
  // TODO bl incorrect
  return this.buffer_.length ?
      new r5js.ast.Character(this.buffer_.shift().writeValue.charAt(0)) :
      null;
};



/**
 * @param {!r5js.InMemoryPortBuffer} buffer
 * @implements {r5js.OutputSavingPort}
 * @struct
 * @constructor
 */
r5js.InMemoryOutputPort = function(buffer) {
  /** @const @private */ this.buffer_ = buffer;
};
r5js.OutputPort.addImplementation(r5js.InMemoryOutputPort);


/** @override */
r5js.InMemoryOutputPort.prototype.write = function(value) {
  this.buffer_.push(value);
};


/** @override */
r5js.InMemoryOutputPort.prototype.getAndClearOutput = function() {
  var values = [];
  for (var i = 0; i < this.buffer_.length; ++i) {
    values.push(this.buffer_[i]);
  }
  this.buffer_.length = 0;
  return values;
};


/** @override */
r5js.InMemoryOutputPort.prototype.close = goog.nullFunction;
