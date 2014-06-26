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

goog.provide('r5js.platform.Html5');


goog.require('goog.Promise');
goog.require('goog.labs.net.xhr');
goog.require('r5js.InMemoryInputPort');
goog.require('r5js.InMemoryOutputPort');
goog.require('r5js.InMemoryPortBuffer');
goog.require('r5js.OutputPort');
goog.require('r5js.SchemeSources');
goog.require('r5js.platform.html5.Client');
goog.require('r5js.platform.html5.Terminal');
goog.require('r5js.replutil');
goog.require('r5js.test.SchemeSources');



/**
 * @param {?} jqConsole
 * @implements {r5js.Platform}
 * @struct
 * @constructor
 */
r5js.platform.Html5 = function(jqConsole) {
  /** @const @private */ this.jqConsole_ = jqConsole;
  /** @const @private {!Object.<string, !r5js.InMemoryPortBuffer>} */
  this.buffers_ = {};
};


/** @override */
r5js.platform.Html5.prototype.exit = goog.nullFunction;


/**
 * @param {!r5js.InputPort=} opt_inputPort
 * @param {!r5js.OutputPort=} opt_outputPort
 * @return {!goog.Promise.<!r5js.Evaluator>}
 * @override TODO bl why is it necessary to repeat the doc?
 */
r5js.platform.Html5.prototype.newEvaluator =
    function(opt_inputPort, opt_outputPort) {
  return goog.Promise.resolve(
      /** @type {!r5js.Evaluator} */(new r5js.platform.html5.Client(
          '../src/js/platform/html5/worker.js',
          opt_outputPort || r5js.OutputPort.NULL)));
};


/** @override */
r5js.platform.Html5.prototype.newInputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = new r5js.InMemoryPortBuffer();
  }
  return new r5js.InMemoryInputPort(this.buffers_[name]);
};


/** @override */
r5js.platform.Html5.prototype.newOutputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = new r5js.InMemoryPortBuffer();
  }
  return new r5js.InMemoryOutputPort(this.buffers_[name]);
};


/** @override */
r5js.platform.Html5.prototype.getTerminal = function() {
  return new r5js.platform.html5.Terminal(
      this.jqConsole_, function(line) {
        return goog.Promise.resolve(r5js.replutil.isLineComplete(line));
      });
};


/** @override */
r5js.platform.Html5.prototype.getSources = function() {
  return r5js.SchemeSources.get(goog.labs.net.xhr.get);
};


/** @override */
r5js.platform.Html5.prototype.getTestSources = function() {
  return r5js.test.SchemeSources.get(goog.labs.net.xhr.get);
};

