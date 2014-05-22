goog.provide('r5js.js.NodeEnvironment');


goog.require('goog.Promise');
goog.require('r5js.InMemoryInputPort');
goog.require('r5js.InMemoryOutputPort');



/**
 * NodeJS-specific environment facilities.
 *
 * TODO bl: The main benefit of running the interpreter in Node
 * over a browser is filesystem access: open-input-file and open-output-file
 * should be connected to the local filesystem.
 *
 * However, the current implementation merely uses in-memory ports.
 * The reason is that the R5RS I/O facilities are underspecified to such
 * an extent as to be of little use to the programmer. (For example,
 * it is unspecified whether calling open-output-file on an existing file
 * truncates or appends, and the effect of concurrent modifications
 * to a file isn't even discussed.)
 *
 * Proper filesystem access through Node will be added for R6RS.
 * @implements {r5js.js.Environment}
 * @struct
 * @constructor
 */
r5js.js.NodeEnvironment = function() {
  /** @const @private {!Object.<string, !r5js.InMemoryPortBuffer>} */
  this.buffers_ = {};
};


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
  if (!(name in this.buffers_)) {
    this.buffers_[name] = [];
  }
  return new r5js.InMemoryInputPort(this.buffers_[name]);
};


/** @override */
r5js.js.NodeEnvironment.prototype.newOutputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = [];
  }
  return new r5js.InMemoryOutputPort(this.buffers_[name]);
};


/** @override */
r5js.js.NodeEnvironment.prototype.getTerminal = function() {
  return new r5js.js.NodeEnvironment.Terminal_();
};



/**
* @implements {r5js.Terminal}
* @struct
    * @constructor
* @private
* @suppress {checkTypes} TODO bl my Node version is 0.6x, but the externs
* I'm using are for 0.10x, which has an incompatible readline API.
*/
r5js.js.NodeEnvironment.Terminal_ = function() {
  var readline = require('readline');
  /** @private @const */
  this.readline_ = readline.createInterface(process.stdin, process.stdout);
  this.readline_.setPrompt(
      r5js.js.NodeEnvironment.Terminal_.PROMPT_,
      r5js.js.NodeEnvironment.Terminal_.PROMPT_.length);
  this.readline_.on('close', this.handleClose_.bind(this));
  this.readline_.prompt();
};


/** @override */
r5js.js.NodeEnvironment.Terminal_.prototype.getNextLineOfInput = function() {
  return new goog.Promise(function(resolve) {
    this.readline_.once('line', resolve);
  }, this);
};


/** @override */
r5js.js.NodeEnvironment.Terminal_.prototype.print = function(str) {
  console.log(str);
  this.readline_.prompt(); // TODO bl double-prompts on Scheme output
};


/** @override */
r5js.js.NodeEnvironment.Terminal_.prototype.error = function(str) {
  console.error(str);
  this.readline_.prompt(); // TODO bl double-prompts on Scheme output
};


/** @private */
r5js.js.NodeEnvironment.Terminal_.prototype.handleClose_ = function() {
  process.exit(0);
};


/** @const @private */ r5js.js.NodeEnvironment.Terminal_.PROMPT_ = '>> ';



