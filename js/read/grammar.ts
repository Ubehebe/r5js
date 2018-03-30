import {Rule} from './rule';

export interface Grammar {
  ruleFor(nonterminal: string /* TODO: should be Nonterminal*/): Rule;
}