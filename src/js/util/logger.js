/* Copyright 2011-2013 Brendan Linn

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


goog.provide('r5js.util.Logger');


goog.require('goog.debug.Logger');


/**
 * General-purpose logging interface.
 * {@link goog.debug.Logger} in the Closure Library is perfectly suitable;
 * I'm using this just to avoid spreading dependencies on Closure throughout
 * the interpreter.
 * @interface
 */
r5js.util.Logger = function() {};


/**
 * @param {string} msg Message to log.
 */
r5js.util.Logger.prototype.fine = function(msg) {};


/**
 * @param {string} msg Message to log.
 */
r5js.util.Logger.prototype.info = function(msg) {};


/**
 * @param {string} msg Message to log.
 */
r5js.util.Logger.prototype.warning = function(msg) {};


/**
  * @param {string} msg Message to log.
 */
r5js.util.Logger.prototype.severe = function(msg) {};


/**
 * Logger implementation that delegates to a {@link goog.debug.Logger}.
 * @implements {r5js.util.Logger}
 * @constructor
 */
r5js.util.LoggerImpl = function(name) {
    /**
     * @type {!goog.debug.Logger}
     * @private
     */
    this.logger_ = goog.debug.Logger.getLogger(name);
};


/**
 * @suppress {undefinedVars} for console
 * @override
 */
r5js.util.LoggerImpl.prototype.fine = function(msg) {
    console.log(msg);
    this.logger_.fine(msg);
};


/**
 * @suppress {undefinedVars} for console
 * @override
 */
r5js.util.LoggerImpl.prototype.info = function(msg) {
    console.log(msg);
    this.logger_.info(msg);
};


/**
 * @suppress {undefinedVars} for console
 * @override
 */
r5js.util.LoggerImpl.prototype.warning = function(msg) {
    console.log(msg);
    this.logger_.warning(msg);
};


/**
 * @suppress {undefinedVars} for console
 * @override
 */
r5js.util.LoggerImpl.prototype.severe = function(msg) {
    this.logger_.severe(msg);
};


/**
 * @param {string} name Name of the logger to get.
 * @return {!r5js.util.Logger} Logger.
 */
r5js.util.Logger.getLogger = function(name) {
    return new r5js.util.LoggerImpl(name);
};