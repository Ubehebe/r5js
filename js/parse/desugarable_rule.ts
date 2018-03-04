import {Rule} from "./parse_rule";

/**
 * A desugarable rule is a rule that has a {@link #desugar} method.
 * This method allows the parser to specify post-parsing actions ("desugaring")
 * on the successfully parsed AST. The generic type of the desugarable rule
 * is the type of the datum passed to the desugar function.
 */
export class DesugarableRule<T> extends Rule {

  constructor() {
    super();
  }

  desugar(desugarFn: (datum: T, env: IEnvironment) => any): DesugarableRule<T> /* TODO polymorphic this */ {
    return this;
  }
}
