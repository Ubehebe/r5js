goog.provide('r5js.InMemoryOutputPort');

goog.require('r5js.InMemoryPortBuffer');
goog.require('r5js.InputPort');
goog.require('r5js.OutputPort');
goog.require('r5js.OutputSavingPort');
goog.require('r5js.ReaderImpl');
goog.require('r5js.ast.Character');
goog.require('r5js.valutil');

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
