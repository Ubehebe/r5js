import {Nonterminal} from "./nonterminals";
import {Rule} from "./parse_rule";

/** TODO: this is almost the same as read/grammar.ts. Unify. */
export interface Grammar {
  ruleFor(nonterminal: Nonterminal): Rule;
}
