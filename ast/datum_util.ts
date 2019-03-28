import {Error} from "../base/error";
import {Value} from "../base/value";
import {Boolean} from './boolean';
import {Datum} from './datum';
import {Identifier} from './identifier';
import {Number} from './number';

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
 */
export function wrapValue(result: Value): Datum {
  switch (typeof result) {
    case 'boolean':
      return new Boolean(result as boolean);
    case 'number':
      return new Number(result as number);
    case 'object':
      return result as Datum;
    case 'string':
      return new Identifier(result as string);
    default:
      throw Error.internalInterpreterError(
          `cannot deduce type from value ${result}: noninjective mapping from values to types`);
  }
}
