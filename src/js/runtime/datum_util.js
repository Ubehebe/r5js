goog.module('r5js.datumutil');

const Boolean = goog.require('r5js.ast.Boolean');
const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const Error = goog.require('r5js.Error');
const Identifier = goog.require('r5js.ast.Identifier');
const List = goog.require('r5js.ast.List');
const Number = goog.require('r5js.ast.Number');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const Nonterminals = goog.require('r5js.parse.Nonterminal').Nonterminals;
const Terminals = goog.require('r5js.parse.Terminals');
const Value = goog.require('r5js.runtime.Value');

/**
 * Munges definitions to get them in a form suitable for let-type bindings.
 * Example:
 * (define (foo x y z) ...) => (foo (lambda (x y z) ...))
 * @param {!CompoundDatum} datum Datum to extract the definition from.
 * TODO bl: you can't extract a definition from an arbitrary datum.
 * Make more strongly typed.
 * @return {!CompoundDatum} A datum representing the given datum's definition.
 * @suppress {checkTypes} for setNextSibling(null)
 */
function extractDefinition(datum) {
  let variable = datum.at(Nonterminals.VARIABLE);
  if (variable) {
    const expr = datum.at(Nonterminals.EXPRESSION);
    variable.setNextSibling(null); // TODO bl
    return new SiblingBuffer()
        .appendSibling(variable)
        .appendSibling(/** @type {!Datum} */(expr))
        .toList(List);
  } else {
    const formalsList = datum.getFirstChild().getNextSibling();
    variable = formalsList.getFirstChild();
    const bodyStart = formalsList.getNextSibling();
    formalsList.setFirstChild(formalsList.getFirstChild().getNextSibling());
    const lambda = prepareLambdaForDefinition_(bodyStart, formalsList);
    variable.setNextSibling(null); // TODO bl
    return new SiblingBuffer()
        .appendSibling(variable)
        .appendSibling(lambda)
        .toList(List);
  }
}

/**
 * @param {!Datum} bodyStart
 * @param {!Datum} formalsList
 * @return {!Datum}
 * @private
 * @suppress {checkTypes} for setNextSibling(null)
 */
function prepareLambdaForDefinition_(bodyStart, formalsList) {
  const buffer = new SiblingBuffer();
  buffer.appendSibling(new Identifier(Terminals.LAMBDA));
  if (formalsList.isImproperList()
      && !formalsList.getFirstChild().getNextSibling()) {
    buffer.appendSibling(new Identifier(
        /** @type {string} */ (formalsList.getFirstChild().getPayload())));
  } else {
    formalsList.setNextSibling(null);
    buffer.appendSibling(formalsList);
  }
  buffer.appendSibling(bodyStart);
  return buffer.toList(List);
}

/**
 * Environments bind names to values, and this implementation represents
 * values idiomatically in JavaScript. For example, the Scheme value #f
 * is represented in environment bindings by the JavaScript value false.
 *
 * However, in a few cases it is useful to retrieve a value from an environment
 * and then wrap it in an AST node. For example, during evaluation of a varargs
 * procedure call: ((lambda (x . xs) xs) 1 2 3), the "rest args" are rolled up
 * into a list and bound to the identifier xs. The list is a
 * {@link r5js.ast.List}, and its elements must be {@link r5js.Datum} instances,
 * not JavaScript numbers.
 * @see {r5js.VarargsUserDefinedProcedure#bindArgs}
 *
 * @param {!Value} result The value to potentially wrap.
 * @return {!Datum} The value, wrapped in a {@link r5js.Datum}
 * if necessary.
 */
function wrapValue(result) {
  switch (typeof result) {
    case 'boolean':
      return new Boolean(result);
    case 'number':
      return new Number(result);
    case 'object':
      return /** @type {!Datum} */ (result);
    case 'string':
      return new Identifier(result);
    default:
      throw Error.internalInterpreterError(
          'cannot deduce type from value ' +
          result +
          ': noninjective mapping from values to types');
  }
}

exports.extractDefinition = extractDefinition;
exports.wrapValue = wrapValue;