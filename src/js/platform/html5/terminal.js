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


goog.provide('r5js.platform.html5.Terminal');


goog.require('goog.Promise');
goog.require('r5js.Terminal');



/**
 * @param {?} jqconsole
 * @param {function(string):!goog.Promise<boolean>} isLineComplete Function
 * to determine if a given line of user input is complete (= ready to be
 * evaluated).
 * @implements {r5js.Terminal}
 * @struct
 * @constructor
 */
r5js.platform.html5.Terminal = function(jqconsole, isLineComplete) {
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
r5js.platform.html5.Terminal.prototype.multilineCallback_ = function(
    line, cb) {
  this.isLineComplete_(line).then(function(lineComplete) {
    cb(lineComplete ? false : 0);
  });
};


/**
 * @override
 * @suppress {checkTypes} for the jqconsole integration
 */
r5js.platform.html5.Terminal.prototype.getNextLineOfInput = function() {
  return new goog.Promise(function(resolve) {
    this.jqconsole_['Prompt'](
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
r5js.platform.html5.Terminal.prototype.print = function(msg) {
  this.jqconsole_['Write'](msg + '\n', 'jqconsole-output');
};


/**
 * @override
 * @suppress {checkTypes} for the jqconsole integration
 */
r5js.platform.html5.Terminal.prototype.error = function(msg) {
  this.jqconsole_['Write'](msg + '\n', 'jqconsole-error');
};
