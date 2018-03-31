import {Rule} from './rule';
import {Nonterminal} from "../parse/nonterminals";

export interface Grammar {
  ruleFor(nonterminal: Nonterminal): Rule;
}