import {Datum, VACUOUS_PROGRAM} from '../ast/datum';
import {List} from '../ast/list';
import {Quasiquote} from '../ast/quasiquote';
import {Quote} from '../ast/quote';
import {SiblingBuffer} from '../ast/sibling_buffer';
import {SimpleDatum} from '../ast/simple_datum';
import {Unquote} from '../ast/unquote';
import {UnquoteSplicing} from '../ast/unquote_splicing';
import {Nonterminal} from '../parse/nonterminals';
import {QUASIQUOTE, QUOTE, UNQUOTE, UNQUOTE_SPLICING} from '../parse/terminals';
import {TokenStream} from '../scan/token_stream';
import {Grammar} from './grammar';
import {Rule} from './rule';

export class RuleFactory {

  constructor(private readonly grammar: Grammar) {}

  one(terminalOrNonterminal: string | Nonterminal): Rule {
    return new One(terminalOrNonterminal, this.grammar);
  }

  zeroOrMore(nonterminal: Nonterminal): Rule {
    return new AtLeast(nonterminal, 0, this.grammar);
  }

  oneOrMore(nonterminal: Nonterminal): Rule {
    return new AtLeast(nonterminal, 1, this.grammar);
  }

  onePrimitive<T>(ctor: new (payload: T) => SimpleDatum<T>): Rule {
    return new OnePrimitive(ctor);
  }

  seq(...rules: Rule[]): Seq {
    return new Seq(rules);
  }

  choice(...rules: Rule[]): Rule {
    return new Choice(rules);
  }
}

class One extends Rule {

  private readonly type: Nonterminal|string;
  private readonly grammar: Grammar;

  constructor(type: string | Nonterminal, grammar: Grammar) {
    super();
    this.type = type;
    this.grammar = grammar;
  }

  /** @override */
  match(tokenStream: TokenStream): Datum | null {
    // The rule will be found in the grammar iff it is a nonterminal.
    const rule = this.type instanceof Nonterminal
        ? this.grammar.ruleFor(this.type)
        : null;
    return rule ? rule.match(tokenStream) : this.matchTerminal(tokenStream);
  }

  private matchTerminal(tokenStream: TokenStream): Datum | null {
    const token = tokenStream.nextToken();
    return token === this.type ? TERMINAL_SENTINEL : null;
  }
}

/**
 * When a terminal is successfully matched out of the token stream,
 * the rule needs to communicate success (= a non-null return value),
 * but the terminal itself is not useful. This sentinel is returned by
 * {@link r5js.read.bnf.One_#matchTerminal} to avoid instantiating
 * useless datums.
 * TODO bl: this API can be improved.
 */
const TERMINAL_SENTINEL = new Datum();

class AtLeast extends Rule {

  private readonly type: Nonterminal;
  private readonly repetition: number;

  constructor(
      type: Nonterminal,
      minRepetitions: number,
      private readonly grammar: Grammar) {
    super();
    this.type = type;
    this.repetition = minRepetitions;
  }

  /** @override */
  match(tokenStream: TokenStream): Datum | null {
    const siblingBuffer = new SiblingBuffer();
    const rule = this.grammar.ruleFor(this.type);
    const checkpoint = tokenStream.checkpoint();
    let num = 0, cur;

    while (cur = rule.match(tokenStream)) {
      siblingBuffer.appendSibling(cur);
      ++num;
    }

    if (num >= this.repetition) {
      // In the special case when repetition_ is 0 and 0 datums were (successfully) matched,
      // siblingBuffer.toSiblings() will return null. However, null is used by this API
      // to communicate failure, so we must  return a different object.
      return siblingBuffer.toSiblings() || VACUOUS_PROGRAM;
    } else {
      tokenStream.restore(checkpoint);
      return null;
    }
  }
}

class OnePrimitive<T> extends Rule {

  constructor(private readonly ctor: new (payload: T) => SimpleDatum<T>) {
    super();
  }

  /** @override */
  match(tokenStream: TokenStream): Datum | null {
    const token = tokenStream.nextToken();
    return token instanceof this.ctor ? token : null;
  }
}

export class Seq extends Rule {

  private ctor: (new (d: Datum) => Datum) | null = null;

  constructor(private readonly rules: Rule[]) {
    super();
  }

  named(ctor: new (d: Datum) => Datum): this {
    this.ctor = ctor;
    return this;
  }

  /** @override */
  match(tokenStream: TokenStream): Datum | null {
    const siblingBuffer = new SiblingBuffer();
    const checkpoint = tokenStream.checkpoint();
    for (let i = 0; i < this.rules.length; ++i) {
      const rule = this.rules[i];
      const parsed = rule.match(tokenStream);
      if (parsed === TERMINAL_SENTINEL) {
        continue;
      } else if (parsed === VACUOUS_PROGRAM) {
        continue;
      } else if (parsed) {
        siblingBuffer.appendSibling(parsed);
      } else {
        tokenStream.restore(checkpoint);
        return null;
      }
    }

    return maybeCanonicalize(siblingBuffer.toList(this.ctor!));
  }
}

/**
 * (quote x) -> 'x
 * (quasiquote x) -> `x
 * (unquote x) -> ,x
 * (unquote-splicing x) -> ,@x
 */
function maybeCanonicalize(datum: Datum): Datum {
  if (!(datum instanceof List) || !datum.getFirstChild()) {
    return datum;
  }
  const firstChildToStrip = datum.getFirstChild();
  if (!(firstChildToStrip instanceof SimpleDatum)) {
    return datum;
  }
  const realFirstChild = firstChildToStrip.getNextSibling()!;
  switch (firstChildToStrip.getPayload()) {
    case QUOTE:
      return new Quote(realFirstChild);
    case QUASIQUOTE:
      return new Quasiquote(realFirstChild);
    case UNQUOTE:
      return new Unquote(realFirstChild);
    case UNQUOTE_SPLICING:
      return new UnquoteSplicing(realFirstChild);
    default:
      return datum;
  }
}

class Choice extends Rule {

  constructor(private readonly rules: Rule[]) {
    super();
  }

  /** @override */
  match(tokenStream: TokenStream): Datum | null {
    for (let i = 0; i < this.rules.length; ++i) {
      const checkpoint = tokenStream.checkpoint();
      let newDatum;
      if (newDatum = this.rules[i].match(tokenStream)) {
        return newDatum;
      } else {
        tokenStream.restore(checkpoint);
      }
    }
    return null;
  }
}
