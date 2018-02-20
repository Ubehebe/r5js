goog.module('r5js.datumutil');

const {Boolean} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/boolean');
const {Datum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {Identifier} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');
const Number = goog.require('r5js.ast.Number');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');

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

exports = {wrapValue};