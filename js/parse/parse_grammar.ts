import {Nonterminal} from "../parse/nonterminals";
import {Rule} from "./parse_rule";

export class /* TODO should be interface */ Grammar {
  ruleFor(nonterminal: Nonterminal): Rule {
    return {} as Rule;
  }
}
