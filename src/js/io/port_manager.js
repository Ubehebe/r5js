/* Copyright 2011-2015 Brendan Linn

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


goog.provide('r5js.PortManager');


goog.require('r5js.InMemoryInputPort');
goog.require('r5js.InMemoryOutputPort');
goog.require('r5js.InMemoryPortBuffer');



/**
 * @struct
 * @constructor
 */
r5js.PortManager = function() {
  /** @const @private {!Object<string, !r5js.InMemoryPortBuffer>} */
  this.buffers_ = {};
};


/**
 * @param {string} name
 * @return {!r5js.InputPort}
 */
r5js.PortManager.prototype.newInputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = new r5js.InMemoryPortBuffer();
  }
  return new r5js.InMemoryInputPort(this.buffers_[name]);
};


/**
 * @param {string} name
 * @return {!r5js.OutputPort}
 */
r5js.PortManager.prototype.newOutputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = new r5js.InMemoryPortBuffer();
  }
  return new r5js.InMemoryOutputPort(this.buffers_[name]);
};

