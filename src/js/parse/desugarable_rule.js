goog.module('r5js.parse.bnf.DesugarableRule');

const IEnvironment = goog.require('r5js.IEnvironment');
const Rule = goog.require('r5js.parse.bnf.Rule');

/**
 * A desugarable rule is a rule that has a {@link #desugar} method.
 * This method allows the parser to specify post-parsing actions ("desugaring")
 * on the successfully parsed AST. The generic type of the desugarable rule
 * is the type of the datum passed to the desugar function.
 * @interface
 * @extends {Rule}
 * @template T
 */
class DesugarableRule {
    /**
     * @param {function(T, !IEnvironment)} desugarFn
     * @return {!DesugarableRule<T>} This rule, for chaining.
     */
    desugar(desugarFn) {}
}

exports = DesugarableRule;
