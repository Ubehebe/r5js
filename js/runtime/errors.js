goog.module('r5js.runtime.errors');
goog.module.declareLegacyNamespace();

const Error = goog.require('r5js.Error');
const Type = goog.require('r5js.Type');
const Value = goog.require('r5js.runtime.Value');

/**
 * @param {!Value} arg The argument.
 * @param {number} argIndex The position of the argument in the argument list (zero-indexed).
 * @param {string} procName The procedure that the interpreter was invoking when this error
 * occurred.
 * @param {!Type} expectedType The type of the argument that the interpreter expected.
 * @param {!Type} actualType The actual type of the argument.
 * @return {!Error}
 */
function argumentTypeError(arg, argIndex, procName, expectedType, actualType) {
  return new Error(
    Error.Type.ARGUMENT_TYPE_ERROR,
    procName + ': argument ' + argIndex + ': want ' +
    expectedType + ', got ' + actualType);
}

/**
 * @param {string} name Error message.
 * @param {!Type=} opt_actualType
 * @return {!Error}
 */
function notAProcedure(name, opt_actualType) {
  return new Error(Error.Type.NOT_A_PROCEDURE, name, opt_actualType);
}

exports = { argumentTypeError, notAProcedure };