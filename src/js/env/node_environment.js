goog.provide('r5js.js.NodeEnvironment');


goog.require('goog.Promise');
goog.require('r5js.EvalAdapter');
goog.require('r5js.IOError');
goog.require('r5js.InputPort');
goog.require('r5js.OutputPort');
goog.require('r5js.runtime.EOF');



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
  return new r5js.js.NodeEnvironment.InputPort_(name);
};


/** @override */
r5js.js.NodeEnvironment.prototype.newOutputPort = function(name) {
  return new r5js.js.NodeEnvironment.OutputPort_(name);
};



/**
 * @param {string} filename
 * @param {string} mode
 * @struct
 * @constructor
 * @private
 * @suppress {checkTypes} TODO bl the compiler complains that Buffer
 * is non-instantiable.
 */
r5js.js.NodeEnvironment.Port_ = function(filename, mode) {
  var fs = require('fs');
  /** @const @protected */ this.fd = fs.openSync(filename, mode);
  /** @const @protected */ this.buf = new Buffer(1 << 10);
  /** @protected */ this.offset = 0;
  /** @protected */ this.position = 0;
};


/** Closes the port. */
r5js.js.NodeEnvironment.Port_.prototype.close = function() {
  var fs = require('fs');
  fs.closeSync(this.fd);
};



/**
 * @param {string} filename
 * @implements {r5js.InputPort}
 * @extends {r5js.js.NodeEnvironment.Port_}
 * @struct
 * @constructor
 * @private
 */
r5js.js.NodeEnvironment.InputPort_ = function(filename) {
  goog.base(this, filename, 'r');
};
goog.inherits(
    r5js.js.NodeEnvironment.InputPort_, r5js.js.NodeEnvironment.Port_);
r5js.InputPort.addImplementation(r5js.js.NodeEnvironment.InputPort_);


/** @override */
r5js.js.NodeEnvironment.InputPort_.prototype.isCharReady = function() {
  return true;
};


/** @override */
r5js.js.NodeEnvironment.InputPort_.prototype.peekChar = function() {
  var ans = this.readChar();
  this.position++;
  return ans;
};


/** @override */
r5js.js.NodeEnvironment.InputPort_.prototype.read = function() {
  return r5js.runtime.EOF; // TODO bl implement
};


/** @override */
r5js.js.NodeEnvironment.InputPort_.prototype.readChar = function() {
  var fs = require('fs');
  var nread = fs.readSync(this.fd, this.buf, this.offset, 1, this.position);
  if (nread < 1) {
    throw new r5js.IOError('readSync: read ' + nread);
  }
  return this.buf[this.offset++];
};



/**
 * @param {string} filename
 * @implements {r5js.OutputPort}
 * @extends {r5js.js.NodeEnvironment.Port_}
 * @struct
 * @constructor
 * @private
 */
r5js.js.NodeEnvironment.OutputPort_ = function(filename) {
  goog.base(this, filename, 'w');
};
goog.inherits(
    r5js.js.NodeEnvironment.OutputPort_, r5js.js.NodeEnvironment.Port_);
r5js.OutputPort.addImplementation(r5js.js.NodeEnvironment.OutputPort_);


/**
 * @override
 * @suppress {checkTypes} The Node API docs say that a null position
 * argument means the current position, but the Node externs give the type
 * of that argument as number.
 */
r5js.js.NodeEnvironment.OutputPort_.prototype.write = function(value) {
  var str = r5js.EvalAdapter.toWriteString(value);
  var fs = require('fs');
  fs.writeSync(this.fd, str, 0, str.length, null /* current position */);
};


/**
 * @override
 * @suppress {checkTypes} The Node API docs say that a null position
 * argument means the current position, but the Node externs give the type
 * of that argument as number.
 */
r5js.js.NodeEnvironment.OutputPort_.prototype.display = function(value) {
  var str = r5js.EvalAdapter.toDisplayString(value);
  var fs = require('fs');
  fs.writeSync(this.fd, str, 0, str.length, null /* current position */);
};
