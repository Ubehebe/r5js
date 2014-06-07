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

goog.provide('r5js.Platform');


goog.require('r5js.platform.Html5');
goog.require('r5js.platform.Node');



/**
 * Abstraction of the (JavaScript) platform that the Scheme implementation
 * is running in.
 * @interface
 */
r5js.Platform = function() {};


/**
 * @param {string} url
 * @return {!goog.Promise.<string>}
 */
r5js.Platform.prototype.fetchUrl = function(url) {};


/** @param {number} statusCode */
r5js.Platform.prototype.exit = function(statusCode) {};


/**
 * @param {string} name
 * @return {!r5js.InputPort}
 */
r5js.Platform.prototype.newInputPort = function(name) {};


/**
 * @param {string} name
 * @return {!r5js.OutputPort}
 */
r5js.Platform.prototype.newOutputPort = function(name) {};


/**
 * @param {!r5js.Evaluator} evaluator TODO bl: ideally, {@link r5js.Terminal}
 * implementations would be "dumb", knowing nothing about Scheme.
 * One complication is multiline input, where terminals often show a different
 * prompt if the current line is a continuation of the last line. One of the
 * Terminal implementations, {@link r5js.platform.Html5.Terminal_},
 * has a quirky API that requires the implementation to know whether
 * the current line will complete. This parameter is passed in order to
 * communicate that knowledge.
 * @return {!r5js.Terminal}
 */
r5js.Platform.prototype.getTerminal = function(evaluator) {};


/**
 * @define {string} The environment that the application expects to be running
 * under. This should only be used in {@link r5js.Platform#get}.
 */
goog.define('r5js.PLATFORM', 'html5');


/**
 * @return {!r5js.Platform}
 * @suppress {missingReturn} because adding a default case defeats
 * the compiler's dead code elimination. See comment in function body.
 */
r5js.Platform.get = function() {
  // Because the Closure Compiler does aggressive dead code elimination,
  // this switch is effectively evaluated at compile time, not runtime.
  // r5js.PLATFORM can be defined as a command-line flag to the compiler,
  // so the switch simplifies to string literal comparisons, which can be done
  // by the compiler.
  // Note: small changes in this function (for example, adding a default case)
  // can defeat the compiler's dead code elimination. Modify with care
  // and ensure the size of the compiled JS makes sense.
  switch (r5js.PLATFORM) {
    case 'html5':
      return new r5js.platform.Html5(arguments[0] /* TODO bl improve */);
    case 'node':
      return new r5js.platform.Node();
  }
};
