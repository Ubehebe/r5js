import {DatumStream} from "./datum_stream";
import {Datum} from "../ast/datum";
import {Environment} from "../runtime/environment";

export interface Rule {
  /** @return True iff the parse succeeded. */
  match(datumStream: DatumStream): boolean | Datum /* TODO ??? */;
}

/**
 * A desugarable rule is a rule that has a {@link #desugar} method.
 * This method allows the parser to specify post-parsing actions ("desugaring")
 * on the successfully parsed AST. The generic type of the desugarable rule
 * is the type of the datum passed to the desugar function.
 */
export interface DesugarableRule<T> extends Rule {
  desugar(desugarFn: (datum: T, env: Environment) => any): this;
}