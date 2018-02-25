goog.module('r5js.Reader');

const {Boolean} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/boolean');
const {Character} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/character');
const {Datum} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {Identifier} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');
const {Grammar} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/read/grammar');
const {Number} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/number');
const {Quasiquote} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/quasiquote');
const {Quote} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/quote');
const {Rule} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/read/rule');
const {RuleFactory} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/read/rule_factory');
const {String} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/string');
const {TokenStream} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/token_stream');
const {Unquote} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/unquote');
const {UnquoteSplicing} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/unquote_splicing');
const {Vector} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/vector');
const {DottedList, List} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/list');
const {Error, ErrorType} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');
const {DATUM, DATUMS} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');
const {
    BACKTICK,
    COMMA,
    COMMA_AT,
    DOT,
    LPAREN,
    LPAREN_VECTOR,
    RPAREN,
    TICK
} = require('/js/parse/terminals_collect_es6_sources.es6/node_modules/__main__/js/parse/terminals');

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

class GrammarImpl extends Grammar {

    constructor() {
        super();
    }

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
grammar[DATUM.toString()] = _.choice(
    _.onePrimitive(Identifier),
    _.onePrimitive(Boolean),
    _.onePrimitive(Number),
    _.onePrimitive(Character),
    _.onePrimitive(String),
    _.seq(
        _.one(LPAREN),
        _.zeroOrMore(DATUM),
        _.one(RPAREN))
        .named(List),
    _.seq(
        _.one(LPAREN),
        _.oneOrMore(DATUM),
        _.one(DOT),
        _.one(DATUM),
        _.one(RPAREN))
        .named(DottedList),
    _.seq(
        _.one(LPAREN_VECTOR),
        _.zeroOrMore(DATUM),
        _.one(RPAREN))
        .named(Vector),
    _.seq(
        _.one(TICK),
        _.one(DATUM))
        .named(Quote),
    _.seq(
        _.one(BACKTICK),
        _.one(DATUM))
        .named(Quasiquote),
    _.seq(
        _.one(COMMA),
        _.one(DATUM))
        .named(Unquote),
    _.seq(
        _.one(COMMA_AT),
        _.one(DATUM))
        .named(UnquoteSplicing));


grammar[DATUMS.toString()] = _.zeroOrMore(DATUM);

/** @implements {Reader} */
class Impl {
    /** @param {!TokenStream} tokenStream */
    constructor(tokenStream) {
        /** @const @private */ this.scanner_ = tokenStream;
    }

    /** @override */
    read() {
        const ans = grammar[DATUMS.toString()].match(this.scanner_);
        // All of the input tokens must be consumed for success.
        const nextToken = this.scanner_.nextToken();
        if (nextToken) {
            throw new Error(ErrorType.READ, "read error: " + nextToken);
        }
        return /** @type {!Datum} */ (ans);
    }
}


exports = Reader;
