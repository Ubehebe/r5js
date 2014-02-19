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


goog.provide('r5js.IPipeline');


/**
 * Interface abstracted from {@link r5js.Pipeline}.
 * @interface
 */
r5js.IPipeline = function() {};


/**
 * @param {!r5js.RootEnvironment} rootEnv The root environment to use.
 */
r5js.IPipeline.prototype.setRootEnv = function(rootEnv) {};


/**
 * @param {string} string The string to scan.
 * @return {!r5js.IScanner} A new scanner to scan the given string.
 */
r5js.IPipeline.prototype.scan = function(string) {};


/**
 * @param {!r5js.IScanner} scanner The scanner to read from.
 * @return {!r5js.Datum} Is this correct?
 * TODO bl: figure out the return type.
 */
r5js.IPipeline.prototype.read = function(scanner) {};


/**
 * @param {!r5js.Datum} root The root to desugar.
 * @param {*} lhs Not sure what the type of this is.
 * @return {!r5js.Datum} Don't know this either.
 * TODO bl: figure out the signature.
 */
r5js.IPipeline.prototype.parse = function(root, lhs) {};


/**
 * @param {!r5js.Datum} root The root to desugar.
 * @param {boolean} replMode If true, desugar in repl mode.
 * @return {!r5js.Continuable}
 * TODO bl: figure out what repl mode is.
 */
r5js.IPipeline.prototype.desugar = function(root, replMode) {};


/**
 * @param {!r5js.Continuable} continuable The continuable to evaluate.
 * @param {function()} onOutput Output callback.
 * @param {!r5js.util.Logger} logger Logger for debugging.
 * @return {*} TODO bl what does this return?
 * TODO bl: tighten the types of onOutput and the return param.
 */
r5js.IPipeline.prototype.Eval = function(continuable, onOutput, logger) {};