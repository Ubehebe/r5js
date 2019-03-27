import {Boolean} from "../ast/boolean";
import {Character} from "../ast/character";
import {Datum} from "../ast/datum";
import {Identifier} from "../ast/identifier";
import {DottedList, List} from "../ast/list";
import {Number} from "../ast/number";
import {Quasiquote} from "../ast/quasiquote";
import {Quote} from "../ast/quote";
import {String} from "../ast/string";
import {Unquote} from "../ast/unquote";
import {UnquoteSplicing} from "../ast/unquote_splicing";
import {Vector} from "../ast/vector";
import {Error, ErrorType} from "../error";
import {DATUM, DATUMS, Nonterminal} from "../parse/nonterminals";
import {BACKTICK, COMMA, COMMA_AT, DOT, LPAREN, LPAREN_VECTOR, RPAREN, TICK} from "../parse/terminals";
import {TokenStream} from "../scan/token_stream";
import {Rule} from "./rule";
import {RuleFactory} from "./rule_factory";

export interface Reader {
  /**
   * @return The root of the datum tree.
   * @throws {Error} if reading the tokens into datums was unsuccessful.
   */
  read(): Datum;
}

export function newReader(tokenStream: TokenStream): Reader {
  return new Impl(tokenStream);
}

const grammar: Map<Nonterminal, Rule> = new Map();

const _ = new RuleFactory({ruleFor: (nonterminal) => grammar.get(nonterminal)!});

// <datum> -> <simple datum> | <compound datum>
// <simple datum> -> <boolean> | <number> | <character> | <string> | <symbol>
// <compound datum> -> <list> | <vector>
// <symbol> -> <identifier>
// <list> -> (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
// <vector> -> #(<datum>*)
// <abbreviation> -> <abbrev prefix> <datum>
// <abbrev prefix> -> ' | ` | , | ,@
grammar.set(DATUM, _.choice(
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
        .named(UnquoteSplicing)));

grammar.set(DATUMS, _.zeroOrMore(DATUM));

class Impl implements Reader {

  private readonly scanner: TokenStream;

  constructor(tokenStream: TokenStream) {
    this.scanner = tokenStream;
  }

  /** @override */
  read(): Datum {
    const ans = grammar.get(DATUMS)!.match(this.scanner);
    // All of the input tokens must be consumed for success.
    const nextToken = this.scanner.nextToken();
    if (nextToken) {
      throw new Error(ErrorType.READ, "read error: " + nextToken);
    }
    return ans!;
  }
}
