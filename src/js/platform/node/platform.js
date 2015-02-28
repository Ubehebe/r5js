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

goog.provide('r5js.curPlatform');


goog.require('goog.Promise');
goog.require('r5js.InMemoryInputPort');
goog.require('r5js.InMemoryOutputPort');
goog.require('r5js.InMemoryPortBuffer');
goog.require('r5js.Platform');
goog.require('r5js.SchemeSources');
goog.require('r5js.boot');
goog.require('r5js.platform.node.Evaluator');
goog.require('r5js.platform.node.Terminal');
goog.require('r5js.test.SchemeSources');



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
 * @implements {r5js.Platform}
 * @struct
 * @constructor
 * @private
 */
r5js.platform.Node_ = function() {
  /** @const @private {!Object<string, !r5js.InMemoryPortBuffer>} */
  this.buffers_ = {};
};


/**
 * @param {string} url
 * @return {!goog.Promise<string>}
 * @private
 */
r5js.platform.Node_.fetchUrl_ = function(url) {
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
r5js.platform.Node_.prototype.exit = function(statusCode) {
  process.exit(statusCode);
};


/**
 * @param {!r5js.InputPort=} opt_inputPort
 * @param {!r5js.OutputPort=} opt_outputPort
 * @return {!goog.Promise<!r5js.Evaluator>}
 * @override TODO bl why is it necessary to repeat the doc?
 */
r5js.platform.Node_.prototype.newEvaluator =
    function(opt_inputPort, opt_outputPort) {
  return this.getSources().then(function(sources) {
    return r5js.boot(
        sources.syntax,
        sources.procedures,
        this,
        opt_inputPort,
        opt_outputPort);
  }, undefined /* opt_onRejected */, this).then(function(syncEvaluator) {
    return new r5js.platform.node.Evaluator(syncEvaluator);
  });
};


/** @override */
r5js.platform.Node_.prototype.newInputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = new r5js.InMemoryPortBuffer();
  }
  return new r5js.InMemoryInputPort(this.buffers_[name]);
};


/** @override */
r5js.platform.Node_.prototype.newOutputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = new r5js.InMemoryPortBuffer();
  }
  return new r5js.InMemoryOutputPort(this.buffers_[name]);
};


/** @override */
r5js.platform.Node_.prototype.getTerminal = function() {
  return new r5js.platform.node.Terminal();
};


/** @override */
r5js.platform.Node_.prototype.getSources = function() {
  return r5js.SchemeSources.get(r5js.platform.Node_.fetchUrl_);
};


/** @override */
r5js.platform.Node_.prototype.getTestSources = function() {
  return r5js.test.SchemeSources.get(r5js.platform.Node_.fetchUrl_);
};


/** @return {!r5js.Platform} */
r5js.curPlatform = function() {
  return new r5js.platform.Node_();
};



