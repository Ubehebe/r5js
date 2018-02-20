goog.module('r5js.Reader');

const Boolean = goog.require('r5js.ast.Boolean');
const Character = goog.require('r5js.ast.Character');
const {Datum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {Identifier} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');
const Grammar = goog.require('r5js.read.Grammar');
const Number = goog.require('r5js.ast.Number');
const Quasiquote = goog.require('r5js.ast.Quasiquote');
const Quote = goog.require('r5js.ast.Quote');
const Rule = goog.require('r5js.read.bnf.Rule');
const RuleFactory = goog.require('r5js.read.RuleFactory');
const String = goog.require('r5js.ast.String');
const TokenStream = goog.require('r5js.TokenStream');
const Unquote = goog.require('r5js.ast.Unquote');
const UnquoteSplicing = goog.require('r5js.ast.UnquoteSplicing');
const Vector = goog.require('r5js.ast.Vector');
const {DottedList, List} = goog.require('r5js.ast.List');
const {Error, ErrorType} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');
const {Nonterminals} = require('/js/parse/nonterminals_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');
const {Terminals} = require('/js/parse/terminals_collect_es6_sources.es6/node_modules/__main__/js/parse/terminals');

/** @interface */
class Reader {
    /**
     * @return {!Datum} The root of the datum tree.
     * If reading the tokens into datums was unsuccessful, a {@link r5js.ReadError}
     * is thrown.
     * @throws {Error}
     */
    read() {}

    /**
     * @param {!TokenStream} tokenStream
     * @return {!Reader}
     */
    static forTokenStream(tokenStream) {
        return new Impl(tokenStream);
    }
}

/** @implements {Grammar} */
class GrammarImpl {
    /** @override */
    ruleFor(nonterminal) {
        return grammar[nonterminal];
    }
}

/** @const {!Object<string, !Rule>} */ const grammar = {};

const _ = new RuleFactory(new GrammarImpl());

// <datum> -> <simple datum> | <compound datum>
// <simple datum> -> <boolean> | <number> | <character> | <string> | <symbol>
// <compound datum> -> <list> | <vector>
// <symbol> -> <identifier>
// <list> -> (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
// <vector> -> #(<datum>*)
// <abbreviation> -> <abbrev prefix> <datum>
// <abbrev prefix> -> ' | ` | , | ,@
grammar[Nonterminals.DATUM.toString()] = _.choice(
    _.onePrimitive(Identifier),
    _.onePrimitive(Boolean),
    _.onePrimitive(Number),
    _.onePrimitive(Character),
    _.onePrimitive(String),
    _.seq(
        _.one(Terminals.LPAREN),
        _.zeroOrMore(Nonterminals.DATUM),
        _.one(Terminals.RPAREN))
        .named(List),
    _.seq(
        _.one(Terminals.LPAREN),
        _.oneOrMore(Nonterminals.DATUM),
        _.one(Terminals.DOT),
        _.one(Nonterminals.DATUM),
        _.one(Terminals.RPAREN))
        .named(DottedList),
    _.seq(
        _.one(Terminals.LPAREN_VECTOR),
        _.zeroOrMore(Nonterminals.DATUM),
        _.one(Terminals.RPAREN))
        .named(Vector),
    _.seq(
        _.one(Terminals.TICK),
        _.one(Nonterminals.DATUM))
        .named(Quote),
    _.seq(
        _.one(Terminals.BACKTICK),
        _.one(Nonterminals.DATUM))
        .named(Quasiquote),
    _.seq(
        _.one(Terminals.COMMA),
        _.one(Nonterminals.DATUM))
        .named(Unquote),
    _.seq(
        _.one(Terminals.COMMA_AT),
        _.one(Nonterminals.DATUM))
        .named(UnquoteSplicing));


grammar[Nonterminals.DATUMS.toString()] = _.zeroOrMore(Nonterminals.DATUM);

/** @implements {Reader} */
class Impl {
    /** @param {!TokenStream} tokenStream */
    constructor(tokenStream) {
        /** @const @private */ this.scanner_ = tokenStream;
    }

    /** @override */
    read() {
        const ans = grammar[Nonterminals.DATUMS.toString()].match(this.scanner_);
        // All of the input tokens must be consumed for success.
        const nextToken = this.scanner_.nextToken();
        if (nextToken) {
            throw new Error(ErrorType.READ, "read error: " + nextToken);
        }
        return /** @type {!Datum} */ (ans);
    }
}


exports = Reader;
