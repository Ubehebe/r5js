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

goog.provide('r5js.platform.android.main');

goog.require('r5js.InputPort');
goog.require('r5js.Platform');
goog.require('r5js.platform.android.OutputPort');


/** @private {goog.Promise.<!r5js.Evaluator>} */
r5js.platform.android.evaluator_ = null;


/**
 * The main method for the Android port.
 * @param {string} input
 */
r5js.platform.android.main = function(input) {
  if (!r5js.platform.android.evaluator_) {
    r5js.platform.android.evaluator_ = r5js.Platform.get().newEvaluator(
        r5js.InputPort.NULL,
        new r5js.platform.android.OutputPort());
  }
  r5js.platform.android.evaluator_.then(function(evaluator) {
    evaluator.evaluate(input);
  });
};
goog.exportSymbol('EVAL', r5js.platform.android.main);
