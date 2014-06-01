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

goog.provide('r5js.Reader');
goog.provide('r5js.read.grammar');

goog.require('r5js.ast.Boolean');
goog.require('r5js.ast.Character');
goog.require('r5js.ast.DottedList');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Number');
goog.require('r5js.ast.Quasiquote');
goog.require('r5js.ast.Quote');
goog.require('r5js.ast.String');
goog.require('r5js.ast.Unquote');
goog.require('r5js.ast.UnquoteSplicing');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.read.bnf');


goog.scope(function() {
var _ = r5js.read.bnf;


/** @type {!Object.<string, !r5js.read.bnf.Rule>} */
r5js.read.grammar = {};


// <datum> -> <simple datum> | <compound datum>
// <simple datum> -> <boolean> | <number> | <character> | <string> | <symbol>
// <compound datum> -> <list> | <vector>
// <symbol> -> <identifier>
// <list> -> (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
// <vector> -> #(<datum>*)
// <abbreviation> -> <abbrev prefix> <datum>
// <abbrev prefix> -> ' | ` | , | ,@
r5js.read.grammar[r5js.parse.Nonterminals.DATUM.toString()] = _.choice(
    _.onePrimitive(r5js.ast.Identifier),
    _.onePrimitive(r5js.ast.Boolean),
    _.onePrimitive(r5js.ast.Number),
    _.onePrimitive(r5js.ast.Character),
    _.onePrimitive(r5js.ast.String),
    _.seq(
        _.one(r5js.parse.Terminals.LPAREN),
        _.zeroOrMore(r5js.parse.Nonterminals.DATUM),
        _.one(r5js.parse.Terminals.RPAREN)).
    named(r5js.ast.List),
    _.seq(
        _.one(r5js.parse.Terminals.LPAREN),
        _.oneOrMore(r5js.parse.Nonterminals.DATUM),
        _.one(r5js.parse.Terminals.DOT),
        _.one(r5js.parse.Nonterminals.DATUM),
        _.one(r5js.parse.Terminals.RPAREN)).
    named(r5js.ast.DottedList),
    _.seq(
        _.one(r5js.parse.Terminals.LPAREN_VECTOR),
        _.zeroOrMore(r5js.parse.Nonterminals.DATUM),
        _.one(r5js.parse.Terminals.RPAREN)).
    named(r5js.ast.Vector),
    _.seq(
        _.one(r5js.parse.Terminals.TICK),
        _.one(r5js.parse.Nonterminals.DATUM)).
        named(r5js.ast.Quote),
    _.seq(
        _.one(r5js.parse.Terminals.BACKTICK),
        _.one(r5js.parse.Nonterminals.DATUM)).
    named(r5js.ast.Quasiquote),
    _.seq(
        _.one(r5js.parse.Terminals.COMMA),
        _.one(r5js.parse.Nonterminals.DATUM)).
        named(r5js.ast.Unquote),
    _.seq(
        _.one(r5js.parse.Terminals.COMMA_AT),
        _.one(r5js.parse.Nonterminals.DATUM)).
    named(r5js.ast.UnquoteSplicing));


r5js.read.grammar[r5js.parse.Nonterminals.DATUMS.toString()] = _.zeroOrMore(
    r5js.parse.Nonterminals.DATUM);
});  // goog.scope



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
  var ans = r5js.read.grammar[r5js.parse.Nonterminals.DATUMS.toString()].
      match(this.scanner_);
  // All of the input tokens must be consumed for success.
  return this.scanner_.nextToken() ? null : ans;
};
