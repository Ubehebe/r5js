import {Datum} from "../ast/datum";
import {Nonterminal, PROGRAM} from "./nonterminals";

export interface Parser {
  /** @return The root of the parse tree, or null if parsing was unsuccessful. */
  parse(nonterminal: Nonterminal): Datum | null;
}
