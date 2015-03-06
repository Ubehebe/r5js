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


goog.provide('r5js.platform.common.newEvaluator');


goog.require('goog.Promise');
goog.require('r5js.Evaluator');
goog.require('r5js.SchemeSources');
goog.require('r5js.boot');


/**
 * @param {!r5js.InputPort=} opt_inputPort
 * @param {!r5js.OutputPort=} opt_outputPort
 * @return {!goog.Promise<!r5js.Evaluator>}
 */
r5js.platform.common.newEvaluator =
    function(opt_inputPort, opt_outputPort) {
  return r5js.SchemeSources.get().then(function(sources) {
    return r5js.boot(
        sources.syntax,
        sources.procedures,
        opt_inputPort,
        opt_outputPort);
  }).then(function(syncEvaluator) {
    return new r5js.platform.common.Evaluator_(syncEvaluator);
  });
};



/**
 * Evaluator implementation that simply wraps a synchronous evaluator
 * in promises. This is appropriate for most non-web platforms, since these
 * typically can run JavaScript synchronously off the main thread.
 * @param {!r5js.sync.Evaluator} evaluator
 * @implements {r5js.Evaluator}
 * @struct
 * @constructor
 * @private
 */
r5js.platform.common.Evaluator_ = function(evaluator) {
  /** @const @private */ this.evaluator_ = evaluator;
};


/** @override */
r5js.platform.common.Evaluator_.prototype.evaluate = function(input) {
  try {
    return goog.Promise.resolve(this.evaluator_.evaluate(input));
  } catch (e) {
    return goog.Promise.reject(e);
  }
};
