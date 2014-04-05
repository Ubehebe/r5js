goog.provide('r5js.js.NodeEnvironment');


goog.require('goog.Promise');



/**
 * @implements {r5js.js.Environment}
 * @struct
 * @constructor
 */
r5js.js.NodeEnvironment = function() {};


/** @override */
r5js.js.NodeEnvironment.prototype.fetchUrl = function(url) {
  return new goog.Promise(function(resolve, reject) {
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


goog.require('r5js.IOError');



/**
 * @param {string} filename
 * @param {string} mode
 * @implements {r5js.InputPort}
 * @implements {r5js.OutputPort}
 * @struct
 * @constructor
 * @private
 */
r5js.js.NodeEnvironment.Port_ = function(filename, mode) {

  /* We set this inside the constructor instead of the usual way
     so that a ReferenceError isn't thrown during parsing. */
  if (!r5js.js.NodeEnvironment.Port_.prototype.fsModule) {
    try {
      /* Of course, require might be defined but do something other
             than what we expect, which is to import the filesystem module.
             We don't check for that. */
      r5js.js.NodeEnvironment.Port_.prototype.fsModule = require('fs');
    } catch (re) {
      if (re instanceof ReferenceError) {
        throw new r5js.IOError('the JavaScript environment lacks ' +
            'filesystem access required for this IO procedure. ' +
                    '(This probably means you are running in a browser.)');
      }
    }
  }

  this.fd = this.fsModule.openSync(filename, mode);
  this.size = this.fsModule.statSync(filename).size;
  this.offset = 0;
};


/** @override */
r5js.js.NodeEnvironment.Port_.prototype.close = function() {
  this.fsModule.closeSync(this.fd);
};


/** @override */
r5js.js.NodeEnvironment.Port_.prototype.isCharReady = function() {
  return true;
};


/** @override */
r5js.js.NodeEnvironment.Port_.prototype.peekChar = function() {
  return this.fsModule.readSync(this.fd, 1, this.offset)[0];
};


/** @override */
r5js.js.NodeEnvironment.Port_.prototype.readChar = function() {
  return this.fsModule.readSync(this.fd, 1, this.offset++)[0];
};


/** @override */
r5js.js.NodeEnvironment.Port_.prototype.write = function(str) {
  this.fsModule.writeSync(this.fd, str, null);
};
