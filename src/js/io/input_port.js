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

goog.provide('r5js.InputPort');


goog.require('goog.functions');
goog.require('r5js.runtime.EOF');



r5js.InputPort = /** @interface */ class {
 /** @return {boolean} */
 isCharReady() {}

 /**
  * @return {r5js.ast.Character} The next character, or null if there are
  * no more characters.
  */
 peekChar() {}

 /**
  * @return {?r5js.runtime.Value} The next value, or null if there are
  * no more values.
  */
 read() {}

 /**
  * @return {r5js.ast.Character} The next character, or null if there are
  * no more characters.
  */
 readChar() {}

 /** @see R5RS 6.6.1 */
 close() {}
};


/** @const @private */ r5js.InputPort.IMPLEMENTED_BY_PROP_ = '$r5js.InputPort';

/**
 * @param {*} obj
 * @return {boolean}
 * TODO bl temporary shim. Remove.
 */
r5js.InputPort.isImplementedBy = function(obj) {
  return !!(obj && obj[r5js.InputPort.IMPLEMENTED_BY_PROP_]);
};


/** @param {function(new: r5js.InputPort, ...)} ctor */
r5js.InputPort.addImplementation = function(ctor) {
  ctor.prototype[r5js.InputPort.IMPLEMENTED_BY_PROP_] = true;
};



/**
 * An input port that has no available input.
 * @implements {r5js.InputPort}
 * @struct
 * @constructor
 * @private
 */
r5js.InputPort.Null_ = function() {};
r5js.InputPort.addImplementation(r5js.InputPort.Null_);


/** @override */
r5js.InputPort.Null_.prototype.isCharReady = goog.functions.FALSE;


/** @override */
r5js.InputPort.Null_.prototype.peekChar = goog.functions.NULL;


/** @override */
r5js.InputPort.Null_.prototype.read = goog.functions.NULL;


/** @override */
r5js.InputPort.Null_.prototype.readChar = goog.functions.NULL;


/** @override */
r5js.InputPort.Null_.prototype.close = goog.nullFunction;


/** @const {!r5js.InputPort} */
r5js.InputPort.NULL = new r5js.InputPort.Null_();
