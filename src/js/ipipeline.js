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



/** @interface */
r5js.IPipeline = function() {};


/**
 * @param {string} string The string to scan.
 * @return {!r5js.TokenStream} A token stream representing the input string.
 */
r5js.IPipeline.prototype.scan = function(string) {};


/**
 * @param {!r5js.TokenStream} tokenStream A token input stream.
 * @return {r5js.Datum} The root of the datum tree, or null if reading failed.
 */
r5js.IPipeline.prototype.read = function(tokenStream) {};


/**
 * @param {!r5js.Datum} root The root to parse.
 * @param {!r5js.parse.Nonterminal=} opt_nonterminal The nonterminal
 * that should be the root of the parse tree.
 * @return {!r5js.Datum}
 */
r5js.IPipeline.prototype.parse = function(root, opt_nonterminal) {};


/**
 * @param {!r5js.Datum} root The root to desugar.
 * @param {boolean} replMode If true, desugar in repl mode.
 * @return {!r5js.Continuable}
 */
r5js.IPipeline.prototype.desugar = function(root, replMode) {};


/**
 * @param {!r5js.Continuable} continuable The continuable to evaluate.
 * @return {!r5js.runtime.Value}
 */
r5js.IPipeline.prototype.Eval = function(continuable) {};
