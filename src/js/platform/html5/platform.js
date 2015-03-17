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
goog.require('r5js.OutputPort');
goog.require('r5js.Platform');
// TODO bl not needed by this file. Workaround for circular dep elsewhere.
goog.require('r5js.PrimitiveProcedures');
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
 * @private
 */
r5js.platform.Html5_ = function(jqConsole) {
  /** @const @private */ this.jqConsole_ = jqConsole;
};


/** @override */
r5js.platform.Html5_.prototype.exit = goog.nullFunction;


/**
* @return {!r5js.Terminal}
* @package
*/
r5js.platform.Html5_.prototype.getTerminal = function() {
  return new r5js.platform.html5.Terminal(
      this.jqConsole_, function(line) {
        return goog.Promise.resolve(r5js.replutil.isLineComplete(line));
      });
};


/** @override */
r5js.platform.Html5_.prototype.newEvaluator =
    function(opt_inputPort, opt_outputPort) {
  return goog.Promise.resolve(
      new r5js.platform.html5.Client(
          opt_outputPort || r5js.OutputPort.NULL));
};


/** @return {!r5js.Platform} */
r5js.curPlatform = function() {
  return new r5js.platform.Html5_(arguments[0] /* TODO bl improve */);
};

