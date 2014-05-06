goog.provide('r5js.js.NodeEnvironment');


goog.require('goog.Promise');
goog.require('r5js.EvalAdapter');
goog.require('r5js.IOError');



/**
 * @implements {r5js.js.Environment}
 * @struct
 * @constructor
 */
r5js.js.NodeEnvironment = function() {};


/** @override */
r5js.js.NodeEnvironment.prototype.fetchUrl = function(url) {
  return new goog.Promise(function(resolve, reject) {
    // TODO bl: move this declaration to the top of this file, instead of
    // repeating it in each method that needs it. This will require changing
    // the build process to omit files not needed by a particular target.
    var fs = require('fs');
    fs.readFile('.' + url, function(err, data) {
      if (err) {
        reject(err);
      } else {
        resolve(data.toString());
      }
    });
  });
};


/** @override */
r5js.js.NodeEnvironment.prototype.exit = function(statusCode) {
  process.exit(statusCode);
};


/** @override */
r5js.js.NodeEnvironment.prototype.newInputPort = function(name) {
  return new r5js.js.NodeEnvironment.Port_(name, 'r');
};


/** @override */
r5js.js.NodeEnvironment.prototype.newOutputPort = function(name) {
  return new r5js.js.NodeEnvironment.Port_(name, 'w');
};



/**
 * @param {string} filename
 * @param {string} mode
 * @implements {r5js.InputPort}
 * @implements {r5js.OutputPort}
 * @struct
 * @constructor
 * @private
 * @suppress {checkTypes} TODO bl the compiler complains that Buffer
 * is non-instantiable.
 */
r5js.js.NodeEnvironment.Port_ = function(filename, mode) {
  var fs = require('fs');

  /** @const @private */ this.fd_ = fs.openSync(filename, mode);
  /** @const @private */ this.buf_ = new Buffer(1 << 10);
  /** @private */ this.offset_ = 0;
  /** @private */ this.position_ = 0;
};


/** @override */
r5js.js.NodeEnvironment.Port_.prototype.close = function() {
  var fs = require('fs');
  fs.closeSync(this.fd_);
};


/** @override */
r5js.js.NodeEnvironment.Port_.prototype.isCharReady = function() {
  return true;
};


/** @override */
r5js.js.NodeEnvironment.Port_.prototype.peekChar = function() {
  var ans = this.readChar();
  this.position_++;
  return ans;
};


/** @override */
r5js.js.NodeEnvironment.Port_.prototype.readChar = function() {
  var fs = require('fs');
  var nread = fs.readSync(this.fd_, this.buf_, this.offset_, 1, this.position_);
  if (nread < 1) {
    throw new r5js.IOError('readSync: read ' + nread);
  }
  return this.buf_[this.offset_++];
};


/**
 * @override
 * @suppress {checkTypes} The Node API docs say that a null position
 * argument means the current position, but the Node externs give the type
 * of that argument as number.
 */
r5js.js.NodeEnvironment.Port_.prototype.write = function(value) {
  var str = r5js.EvalAdapter.toWriteString(value);
  var fs = require('fs');
  fs.writeSync(this.fd_, str, 0, str.length, null /* current position */);
};
