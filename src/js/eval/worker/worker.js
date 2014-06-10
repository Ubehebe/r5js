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

/**
 * @fileoverview Driver to run the (uncompiled) tests from inside a web worker.
 * @suppress {undefinedVars|globalThis} due to the unusual web worker setup.
 */


// See bootstrap/webworkers.js in the Closure Library for discussion.
CLOSURE_BASE_PATH = '../../../../closure-library/closure/goog/';
importScripts(
    CLOSURE_BASE_PATH + 'bootstrap/webworkers.js',
    CLOSURE_BASE_PATH + 'base.js',
    '../../../../build/deps.js');


// TODO bl: nothing goog.requires this name, but typechecking appears
// not to work for this file unless it has a goog.provide.
goog.provide('r5js.Worker');

goog.require('goog.events.EventType');
goog.require('r5js.boot');
goog.require('r5js.valutil');


/** @private {r5js.sync.Evaluator} */
r5js.Worker.evaluator_;


/**
 * @param {string} input
 * @private
 * Note: this can throw an {@link r5js.Error}, which will be caught by
 * the worker's parent.
 */
r5js.Worker.handleInput_ = function(input) {
  postMessage(
      r5js.valutil.toDisplayString(
      r5js.Worker.evaluator_.evaluate(input)));
};


/**
 * @param {!r5js.test.SchemeSources} sources
 * @private
 */
r5js.Worker.boot_ = function(sources) {
  r5js.Worker.evaluator_ = r5js.boot(sources.syntax, sources.procedures);
};

addEventListener(goog.events.EventType.MESSAGE, function(e) {
  if (goog.isString(e.data)) {
    r5js.Worker.handleInput_(e.data);
  } else {
    r5js.Worker.boot_(/** @type {!r5js.test.SchemeSources} */ (e.data));
  }
}, false);
