import * as Terminals from "./terminals";
import {Grammar} from "./parse_grammar";
import {Nonterminal} from "./nonterminals";
import {DesugarableRule} from "./desugarable_rule";
import {Rule} from "./parse_rule";
import {CompoundDatum} from "../ast/compound_datum";
import {Datum} from "../ast/datum";
import {DatumStream} from "./datum_stream";
import {DottedList, List} from "../ast/list";
import {Vector} from "../ast/vector";
import {Quote} from "../ast/quote";
import {Quasiquote} from "../ast/quasiquote";
import {SimpleDatum} from "../ast/simple_datum";
import {UnquoteSplicing} from "../ast/unquote_splicing";
import {Unquote} from "../ast/unquote";

export class RuleFactory {

  constructor(private readonly grammar: Grammar) {}

  one(terminalOrNonterminal: string | Nonterminal): DesugarableRule<any> {
    return typeof terminalOrNonterminal === 'string'
        ? new OneTerminal(terminalOrNonterminal)
        : new OneNonterminal(terminalOrNonterminal, this.grammar);
  }

  zeroOrMore(nonterminal: Nonterminal): Rule {
    return new AtLeast(nonterminal, 0, this.grammar);
  }

  oneOrMore(nonterminal: Nonterminal): Rule {
    return new AtLeast(nonterminal, 1, this.grammar);
  }

  choice(...rules: Rule[]): Rule {
    return new Choice(rules);
  }

  seq(...rules: Rule[]): DesugarableRule<any> {
    return new Seq(rules);
  }

  list(...rules: Rule[]): DesugarableRule<CompoundDatum> {
    const newRules: Rule[] = [];
    newRules.push(new OneTerminal(Terminals.LPAREN));
    for (let i = 0; i < rules.length; ++i) {
      newRules.push(rules[i]);
    }
    newRules.push(new OneTerminal(Terminals.RPAREN));
    return new Seq(newRules);
  }

  dottedList(beforeDot: Rule, afterDot: Rule): DesugarableRule<CompoundDatum> {
    const rules = [
      new OneTerminal(Terminals.LPAREN),
      beforeDot,
      new OneTerminal(Terminals.DOT),
      afterDot];
    return new Seq(rules);
  }

  vector(...rules: Rule[]): DesugarableRule<CompoundDatum> {
    const newRules: Rule[] = [];
    newRules.push(new OneTerminal(Terminals.LPAREN_VECTOR));
    for (let i = 0; i < rules.length; ++i) {
      newRules.push(rules[i]);
    }
    newRules.push(new OneTerminal(Terminals.RPAREN));
    return new Seq(newRules);
  }

  matchDatum(predicate: (Datum) => boolean): Rule {
    return new MatchDatum(predicate);
  }
}

class OneTerminal extends DesugarableRule<string> {

  constructor(readonly terminal: string) {
    super();
  }

  /**
   * TODO: this class isn't really desugarable. It implements
   * DesugarableRule so that one() can return a DesugurableRule consistently.
   * Break up one() into oneTerminal(), returning a Rule, and oneNonterminal(), returning a
   * DesugarableRule.
   * @override
   */
  desugar(desugarFunc): DesugarableRule<string> {
    return this;
  }

  /**
   * @override
   * TODO bl put the instanceof checks into the Datum subclasses
   */
  match(datumStream) {
    if (this.terminal === Terminals.RPAREN) {
      return datumStream.maybeAdvanceToNextSiblingOfParent();
    }

    const next = datumStream.getNextDatum();
    let match = false;
    switch (this.terminal) {
      case Terminals.LPAREN:
        match = next instanceof List;
        break;
      case Terminals.LPAREN_DOT:
        match = next instanceof DottedList;
        break;
      case Terminals.LPAREN_VECTOR:
        match = next instanceof Vector;
        break;
      case Terminals.TICK:
        match = next instanceof Quote;
        break;
      case Terminals.BACKTICK:
        match = next instanceof Quasiquote;
        break;
      case Terminals.COMMA:
        match = next instanceof Unquote;
        break;
      case Terminals.COMMA_AT:
        match = next instanceof UnquoteSplicing;
        break;
      default: // TODO bl where is this from?
        if (next instanceof SimpleDatum && next.getPayload() === this.terminal) {
          datumStream.advanceToNextSibling();
          return true;
        } else {
          return false;
        }
    }

    if (match) {
      datumStream.advanceToChild();
    }
    return match;
  }
}

class OneNonterminal extends DesugarableRule<Nonterminal> {

  private desugarFunc: ((Datum, IEnvironment) => any) | null = null;

  constructor(
      private readonly nonterminal: Nonterminal,
      private readonly grammar: Grammar) {
    super();
  }

  /** @override */
  desugar(desugarFunc) {
    this.desugarFunc = desugarFunc;
    return this;
  }

  /** @override */
  match(datumStream) {
    const parsed = this.grammar.ruleFor(this.nonterminal).match(datumStream);
    if (parsed instanceof Datum) {
      parsed.setParse(this.nonterminal);
      if (this.desugarFunc) {
        parsed.setDesugar(this.desugarFunc);
      }
      datumStream.advanceTo(parsed.getNextSibling());
    }
    return parsed;
  }
}

class AtLeast extends Rule {

  constructor(
      private readonly nonterminal: Nonterminal,
      private readonly minRepetitions: number,
      private readonly grammar: Grammar) {
    super();
  }

  /** @override */
  match(datumStream) {
    let numParsed = 0;
    let parsed;
    while (parsed = this.grammar.ruleFor(this.nonterminal).match(datumStream)) {
      parsed.setParse(this.nonterminal);
      ++numParsed;
    }
    return numParsed >= this.minRepetitions;
  }
}

class MatchDatum extends Rule {

  constructor(
      private readonly predicate: (Datum) => boolean) {
    super();
  }

  /** @override */
  match(datumStream) {
    const next = datumStream.getNextDatum();
    if (next && this.predicate(next)) {
      datumStream.advanceToNextSibling();
      return true;
    } else {
      return false;
    }
  }
}

class Choice extends Rule {

  constructor(private readonly rules: Rule[]) {
    super();
  }

  /** @override */
  match(datumStream) {
    let parsed;
    for (let i = 0; i < this.rules.length; ++i) {
      const rule = this.rules[i];
      if (parsed = rule.match(datumStream)) {
        return parsed;
      }
    }
    return false;
  }
}

class Seq extends DesugarableRule<CompoundDatum> {

  private readonly rules: Rule[];
  private desugarFunc: ((CompoundDatum, IEnvironment) => any) | null = null;

  constructor(rules: Rule[]) {
    super();
    this.rules = rewriteImproperList(rules);
  }

  /** @override */
  match(datumStream: DatumStream): boolean | Datum {
    const root = datumStream.getNextDatum()!;

    for (let i = 0; i < this.rules.length; ++i) {
      if (!this.rules[i].match(datumStream)) {
        datumStream.advanceTo(root);
        return false;
      }
    }

    // Just in case of an empty program
    const nextSibling = root && root.getNextSibling()!;
    datumStream.advanceTo(nextSibling);

    if (root instanceof Datum && this.desugarFunc) {
      root.setDesugar(this.desugarFunc);
    }

    return root || false;
  }

  /** @override */
  desugar(desugarFunc: (CompoundDatum, IEnvironment) => any): DesugarableRule<CompoundDatum> {
    this.desugarFunc = desugarFunc;
    return this;
  }
}

/**
 * This is a convenience function: we want to specify parse rules like
 * (<variable>+ . <variable>) as if we don't know ahead of time whether
 * the list is going to be dotted or not, but the reader already knows.
 * Proper and improper lists are both represented as first-child-next-sibling
 * linked lists; the only difference is the type ('(' vs. '.('). So we rewrite
 * the parse rules to conform to the reader's knowledge.
 */
function rewriteImproperList(rules: any /* TODO should be Rule[] */): Rule[] {
  // example: (define (x . y) 1) => (define .( x . ) 1)
  /* No RHS in the grammar has more than one dot.
   This will break if such a rule is added. */
  const indexOfDot =
      rules.findIndex(rule => rule instanceof OneTerminal && rule.terminal === Terminals.DOT);

  if (indexOfDot === -1) {
    return rules;
  }

  // Find the closest opening paren to the left of the dot and rewrite it as .(
  for (let i = indexOfDot - 1; i >= 0; --i) {
    const rule = rules[i];
    if (rule instanceof OneTerminal && rule.terminal === Terminals.LPAREN) {
      rules[i] = new OneTerminal(Terminals.LPAREN_DOT);
      break;
    }
  }
  /* Splice out the dot and the datum following the dot -- it has already
   been read as part of the list preceding the dot.
   todo bl: this will cause problems with exactly one part of the grammar:
   <template> -> (<template element>+ . <template>)
   I think it's easier to check for this in the evaluator. */
  rules.splice(indexOfDot, 2);
  return rules;
}