import * as Terminals from "./terminals";
import {Grammar} from "./grammar";
import {Nonterminal} from "./nonterminals";
import {DesugarableRule} from "./desugarable_rule";
import {Rule} from "./rule";
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

  private readonly grammar_: Grammar;

  constructor(grammar) {
    this.grammar_ = grammar;
  }

  one(terminalOrNonterminal: string | Nonterminal): DesugarableRule<any> {
    return typeof terminalOrNonterminal === 'string'
        ? new OneTerminal(terminalOrNonterminal)
        : new OneNonterminal(terminalOrNonterminal, this.grammar_);
  }

  zeroOrMore(nonterminal: Nonterminal): Rule {
    return new AtLeast(nonterminal, 0, this.grammar_);
  }

  oneOrMore(nonterminal: Nonterminal): Rule {
    return new AtLeast(nonterminal, 1, this.grammar_);
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

  readonly terminal_: string;

  constructor(terminal: string) {
    super();
    this.terminal_ = terminal;
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
    if (this.terminal_ === Terminals.RPAREN) {
      return datumStream.maybeAdvanceToNextSiblingOfParent();
    }

    const next = datumStream.getNextDatum();
    let match = false;
    switch (this.terminal_) {
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
        if (next instanceof SimpleDatum && next.getPayload() === this.terminal_) {
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

  private readonly nonterminal_: Nonterminal;
  private readonly grammar_: Grammar;
  private desugarFunc_: ((Datum, IEnvironment) => any) | null;

  constructor(nonterminal: Nonterminal, grammar: Grammar) {
    super();
    this.nonterminal_ = nonterminal;
    this.grammar_ = grammar;
    this.desugarFunc_ = null;
  }

  /** @override */
  desugar(desugarFunc) {
    this.desugarFunc_ = desugarFunc;
    return this;
  }

  /** @override */
  match(datumStream) {
    const parsed = this.grammar_.ruleFor(this.nonterminal_).match(datumStream);
    if (parsed instanceof Datum) {
      parsed.setParse(this.nonterminal_);
      if (this.desugarFunc_) {
        parsed.setDesugar(this.desugarFunc_);
      }
      datumStream.advanceTo(/** @type {!Datum} */ (parsed.getNextSibling()));
    }
    return parsed;
  }
}

class AtLeast extends Rule {

  private readonly nonterminal_: Nonterminal;
  private readonly minRepetitions_: number;
  private readonly grammar_: Grammar;

  constructor(nonterminal: Nonterminal, minRepetitions: number, grammar: Grammar) {
    super();
    this.nonterminal_ = nonterminal;
    this.minRepetitions_ = minRepetitions;
    this.grammar_ = grammar;
  }

  /** @override */
  match(datumStream) {
    let numParsed = 0;
    let parsed;
    while (parsed = this.grammar_.ruleFor(this.nonterminal_).match(datumStream)) {
      parsed.setParse(this.nonterminal_);
      ++numParsed;
    }
    return numParsed >= this.minRepetitions_;
  }
}

class MatchDatum extends Rule {

  private readonly predicate_: (Datum) => boolean;

  constructor(predicate: (Datum) => boolean) {
    super();
    this.predicate_ = predicate;
  }

  /** @override */
  match(datumStream) {
    const next = datumStream.getNextDatum();
    if (next && this.predicate_(next)) {
      datumStream.advanceToNextSibling();
      return true;
    } else {
      return false;
    }
  }
}

class Choice extends Rule {

  private readonly rules_: Rule[];

  constructor(rules: Rule[]) {
    super();
    this.rules_ = rules;
  }

  /** @override */
  match(datumStream) {
    let parsed;
    for (let i = 0; i < this.rules_.length; ++i) {
      const rule = this.rules_[i];
      if (parsed = rule.match(datumStream)) {
        return parsed;
      }
    }
    return false;
  }
}

class Seq extends DesugarableRule<CompoundDatum> {

  private readonly rules_: Rule[];
  private desugarFunc_: ((CompoundDatum, IEnvironment) => any) | null;

  constructor(rules: Rule[]) {
    super();
    this.rules_ = rewriteImproperList(rules);
    this.desugarFunc_ = null;
  }

  /** @override */
  match(datumStream: DatumStream): boolean | Datum {
    const root = datumStream.getNextDatum()!;

    for (let i = 0; i < this.rules_.length; ++i) {
      if (!this.rules_[i].match(datumStream)) {
        datumStream.advanceTo(root);
        return false;
      }
    }

    // Just in case of an empty program
    const nextSibling = root && root.getNextSibling()!;
    datumStream.advanceTo(nextSibling);

    if (root instanceof Datum && this.desugarFunc_) {
      root.setDesugar(this.desugarFunc_);
    }

    return root || false;
  }

  /** @override */
  desugar(desugarFunc: (CompoundDatum, IEnvironment) => any): DesugarableRule<CompoundDatum> {
    this.desugarFunc_ = desugarFunc;
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
      rules.findIndex(rule => rule instanceof OneTerminal && rule.terminal_ === Terminals.DOT);

  if (indexOfDot === -1) {
    return rules;
  }

  // Find the closest opening paren to the left of the dot and rewrite it as .(
  for (let i = indexOfDot - 1; i >= 0; --i) {
    const rule = rules[i];
    if (rule instanceof OneTerminal && rule.terminal_ === Terminals.LPAREN) {
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