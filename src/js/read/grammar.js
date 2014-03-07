goog.provide('r5js.read.grammar');


goog.require('r5js.DatumType');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.read.bnf');



goog.scope(function() {
var _ = r5js.read.bnf;


/** @type {!Object.<!r5js.parse.Nonterminal, !r5js.read.bnf.Rule>} */
r5js.read.grammar = {};


// <datum> -> <simple datum> | <compound datum>
// <simple datum> -> <boolean> | <number> | <character> | <string> | <symbol>
// <compound datum> -> <list> | <vector>
// <symbol> -> <identifier>
// <list> -> (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
// <vector> -> #(<datum>*)
// <abbreviation> -> <abbrev prefix> <datum>
// <abbrev prefix> -> ' | ` | , | ,@
r5js.read.grammar[r5js.parse.Nonterminals.DATUM] = _.choice(
    _.onePrimitive(r5js.DatumType.IDENTIFIER),
    _.onePrimitive(r5js.DatumType.BOOLEAN),
    _.onePrimitive(r5js.DatumType.NUMBER),
    _.onePrimitive(r5js.DatumType.CHARACTER),
    _.onePrimitive(r5js.DatumType.STRING),
    _.seq(
        _.one(r5js.parse.Terminals.LPAREN),
        _.zeroOrMore(r5js.parse.Nonterminals.DATUM).
            named(r5js.parse.Terminals.LPAREN),
        _.one(r5js.parse.Terminals.RPAREN)),
    _.seq(
        _.one(r5js.parse.Terminals.LPAREN),
        _.oneOrMore(r5js.parse.Nonterminals.DATUM).
            named(r5js.DatumType.DOTTED_LIST),
        _.one(r5js.parse.Terminals.DOT),
        _.one(r5js.parse.Nonterminals.DATUM).
            named(r5js.parse.Terminals.LPAREN_DOT),
        _.one(r5js.parse.Terminals.RPAREN)),
    _.seq(
        _.one(r5js.parse.Terminals.LPAREN_VECTOR),
        _.zeroOrMore(r5js.parse.Nonterminals.DATUM).
            named(r5js.parse.Terminals.LPAREN_VECTOR),
        _.one(r5js.parse.Terminals.RPAREN)),
    _.seq(
        _.one(r5js.parse.Terminals.TICK),
        _.one(r5js.parse.Nonterminals.DATUM).named(r5js.parse.Terminals.TICK)),
    _.seq(
        _.one(r5js.parse.Terminals.BACKTICK),
        _.one(r5js.parse.Nonterminals.DATUM).
            named(r5js.parse.Terminals.BACKTICK)),
    _.seq(
        _.one(r5js.parse.Terminals.COMMA),
        _.one(r5js.parse.Nonterminals.DATUM).named(r5js.parse.Terminals.COMMA)),
    _.seq(
        _.one(r5js.parse.Terminals.COMMA_AT),
        _.one(r5js.parse.Nonterminals.DATUM).
            named(r5js.parse.Terminals.COMMA_AT)));


r5js.read.grammar[r5js.parse.Nonterminals.DATUMS] = _.choice(
    _.zeroOrMore(r5js.parse.Nonterminals.DATUM).
        named(r5js.parse.Nonterminals.DATUMS));
});  // goog.scope
