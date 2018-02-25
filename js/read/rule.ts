import {Datum} from '../ast/datum';
import {TokenStream} from '../scan/token_stream';

// TODO should be an interface
export class Rule {
  /**
   * @return The datum extracted from the token stream, or null if
   * reading was unsuccessful. Note that this may not be a proper tree:
   * rules like {@link r5js.read.bnf.AtLeast_} should return a list of siblings.
   */
  match(tokenStream: TokenStream): Datum | null {
    return null;
  }
}