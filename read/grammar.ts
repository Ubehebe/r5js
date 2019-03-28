import {Nonterminal} from "../parse/nonterminals";
import {Rule} from './rule';

export interface Grammar {
  ruleFor(nonterminal: Nonterminal): Rule;
}