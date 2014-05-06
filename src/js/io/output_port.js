/* Copyright 2011, 2012 Brendan Linn

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


goog.provide('r5js.OutputPort');



/** @interface */
r5js.OutputPort = function() {};


/**
 * According to R5RS 6.6.3, write is supposed to write a textual representation
 * of the given value to the output port, including escaping of backslashes
 * and doublequotes. But write can be more useful if it passes the value
 * directly, letting the output port decide how to represent it.
 * @param {!r5js.runtime.Value} value Value to write.
 */
r5js.OutputPort.prototype.write = function(value) {};


/** @see R5RS 6.6.1 */
r5js.OutputPort.prototype.close = function() {};


/** @const @private */
r5js.OutputPort.IMPLEMENTED_BY_PROP_ = '$r5js.OutputPort';


/**
 * @param {*} obj
 * @return {boolean}
 * TODO bl temporary shim. Remove.
 */
r5js.OutputPort.isImplementedBy = function(obj) {
  return !!(obj && obj[r5js.OutputPort.IMPLEMENTED_BY_PROP_]);
};


/** @param {function(new: r5js.OutputPort, ...)} ctor */
r5js.OutputPort.addImplementation = function(ctor) {
  ctor.prototype[r5js.OutputPort.IMPLEMENTED_BY_PROP_] = true;
};



/**
 * An output port that discards its output.
 * @implements {r5js.OutputPort}
 * @struct
 * @constructor
 * @private
 */
r5js.OutputPort.Null_ = function() {};
r5js.OutputPort.addImplementation(r5js.OutputPort.Null_);


/** @override */
r5js.OutputPort.Null_.prototype.close = goog.nullFunction;


/** @override */
r5js.OutputPort.Null_.prototype.write = goog.nullFunction;


/** @const {!r5js.OutputPort} */
r5js.OutputPort.NULL = new r5js.OutputPort.Null_();
