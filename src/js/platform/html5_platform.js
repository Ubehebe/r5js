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
goog.require('r5js.WorkerDriver');
// TODO bl cyclic dependency goog.require('r5js.boot');
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


/** @override */
r5js.platform.Html5.prototype.fetchUrl = goog.labs.net.xhr.get;


/** @override */
r5js.platform.Html5.prototype.newEvaluator = function() {
  return r5js.test.SchemeSources.get(this.fetchUrl.bind(this)).
      then(function(sources) {
        return new r5js.WorkerDriver(
           '../src/js/eval/worker/worker.js', sources);
      });
};


/** @override */
r5js.platform.Html5.prototype.newSyncEvaluator = function() {
  return r5js.test.SchemeSources.get(this.fetchUrl.bind(this)).
      then(function(sources) {
        return r5js.boot(sources.syntax, sources.procedures);
      });
};


/** @override */
r5js.platform.Html5.prototype.newInputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = [];
  }
  return new r5js.InMemoryInputPort(this.buffers_[name]);
};


/** @override */
r5js.platform.Html5.prototype.newOutputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = [];
  }
  return new r5js.InMemoryOutputPort(this.buffers_[name]);
};


/** @override */
r5js.platform.Html5.prototype.getTerminal = function(lineCompleteHandler) {
  return new r5js.platform.Html5.Terminal_(
      this.jqConsole_, lineCompleteHandler);
};



/**
 * @param {?} jqconsole
 * @param {function(string):!goog.Promise.<boolean>} isLineComplete Function
 * to determine if a given line of user input is complete (= ready to be
 * evaluated).
 * @implements {r5js.Terminal}
 * @struct
 * @constructor
 * @private
 */
r5js.platform.Html5.Terminal_ = function(jqconsole, isLineComplete) {
  /** @const @private */ this.jqconsole_ = jqconsole;
  /** @const @private */ this.isLineComplete_ = isLineComplete;
};


/**
 * @param {string} line
 * @param {!Function} cb
 * @private
 * @see https://github.com/replit/jq-console for details on the odd return
 * values.
 */
r5js.platform.Html5.Terminal_.prototype.multilineCallback_ = function(
    line, cb) {
  this.isLineComplete_(line).then(function(lineComplete) {
    cb(lineComplete ? false : 0);
  });
};


/**
 * @override
 * @suppress {checkTypes} for the jqconsole integration
 */
r5js.platform.Html5.Terminal_.prototype.getNextLineOfInput = function() {
  return new goog.Promise(function(resolve) {
    this.jqconsole_.Prompt(
        true /* history_enabled */,
        resolve,
        this.multilineCallback_.bind(this),
        true /* async_multiline */);
  }, this);
};


/**
 * @override
 * @suppress {checkTypes} for the jqconsole integration
 */
r5js.platform.Html5.Terminal_.prototype.print = function(msg) {
  this.jqconsole_.Write(msg + '\n', 'jqconsole-output');
};


/**
 * @override
 * @suppress {checkTypes} for the jqconsole integration
 */
r5js.platform.Html5.Terminal_.prototype.error = function(msg) {
  this.jqconsole_.Write(msg + '\n', 'jqconsole-error');
};

