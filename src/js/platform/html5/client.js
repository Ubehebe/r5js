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


goog.provide('r5js.platform.html5.Client');


goog.require('goog.Promise');
goog.require('goog.events.EventType');



/**
 * @param {string} scriptName
 * @implements {r5js.Evaluator}
 * @struct
 * @constructor
 */
r5js.platform.html5.Client = function(scriptName) {
  /** @const @private */ this.worker_ = new Worker(scriptName);
  this.worker_.postMessage(null);
};


/** @override */
r5js.platform.html5.Client.prototype.evaluate = function(input) {
  var worker = this.worker_;
  return new goog.Promise(function(resolve, reject) {
    var onMessage = function(e) {
      worker.removeEventListener(
          goog.events.EventType.MESSAGE, onMessage, false);
      resolve(e.data);
    };
    var onError = function(e) {
      worker.removeEventListener(
          goog.events.EventType.ERROR, onError, false);
      reject(e.message);
    };
    worker.addEventListener(
        goog.events.EventType.MESSAGE, onMessage, false);
    worker.addEventListener(
        goog.events.EventType.ERROR, onError, false);
    worker.postMessage(input);
  });
};
