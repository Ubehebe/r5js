import {Nonterminal, PROGRAM} from "../parse/nonterminals";
import {Datum} from "../ast/datum";

export class /* TODO should be interface */ Parser {
  /** @return The root of the parse tree, or null if parsing was unsuccessful. */
  parse(nonterminal: Nonterminal = PROGRAM): Datum | null {
    return null;
  }
}
