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


goog.provide('r5js.OutputPort');



/**
 * In Scheme, the main way of doing output is by passing a value to
 * the display and write procedures. (Less important output procedures include
 * write-char and newline.)
 *
 * R5RS 6.6.3 goes into detail about the differences between display and write;
 * it involves quote and backslash escaping, among other things.
 *
 * I think it makes more sense to let the output port implementations decide
 * what display and write do (and whether they do the same thing).
 * An implementation could, for example, convert the Scheme value
 * to some suitable value in the target environment, which need not be
 * a string. This is what {@link r5js.OutputSavingPort} does, for example.
 *
 * @interface
 */
r5js.OutputPort = function() {};


/** @param {!r5js.runtime.Value} value Value to write. */
r5js.OutputPort.prototype.writeValue = function(value) {};


/** @param {string} c Character to write. */
r5js.OutputPort.prototype.writeChar = function(c) {};


/** @param {!r5js.runtime.Value} value Value to display. */
r5js.OutputPort.prototype.display = function(value) {};


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
r5js.OutputPort.Null_.prototype.display = goog.nullFunction;


/** @override */
r5js.OutputPort.Null_.prototype.writeValue = goog.nullFunction;


/** @override */
r5js.OutputPort.Null_.prototype.writeChar = goog.nullFunction;


/** @const {!r5js.OutputPort} */
r5js.OutputPort.NULL = new r5js.OutputPort.Null_();
