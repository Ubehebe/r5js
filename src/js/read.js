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


goog.provide('r5js.Reader');


goog.require('r5js.read.bnf');
goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.read.grammar');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.OutputMode');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.parse.isTerminal');
goog.require('r5js.PrimitiveProcedure');

/**
 * @param {!r5js.TokenStream} scanner
 * @implements {r5js.IReader}
 * @struct
 * @constructor
 */
r5js.Reader = function(scanner) {
    /** @const @private {!r5js.TokenStream} */
    this.scanner_ = scanner;
};


/** @override */
r5js.Reader.prototype.read = function() {
    return r5js.read.grammar[r5js.parse.Nonterminals.DATUMS].match(this.scanner_);
};

/**
 * This is the inverse of {@link r5js.Reader.read}, which is why it's here.
 * @param {!r5js.OutputMode} outputMode Desired output mode.
 * @return {string} String representation for desired output mode.
 */
r5js.Datum.prototype.stringForOutputMode = function(outputMode) {

    var ans, child;
    var endDelimiter = "";

    switch (this.getType()) {
        case r5js.DatumType.FFI: // JavaScript object
            return this.getPayload().toString();
        case null:
            // Mainly for silly stuff like (cons (if #f #f) (display 'hi))
            return 'undefined';
        case r5js.DatumType.IDENTIFIER:
                default:
                    throw new r5js.InternalInterpreterError('unknown datum type ' + this.getType());
            }
    };