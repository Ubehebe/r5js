goog.module('r5js.runtime.errors');

const {Type} = goog.require('r5js.Type');
const {Error, ErrorType} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');

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
    ErrorType.ARGUMENT_TYPE_ERROR,
    procName + ': argument ' + argIndex + ': want ' +
    expectedType + ', got ' + actualType);
}

/**
 * @param {string} name Error message.
 * @param {!Type=} actualType
 * @return {!Error}
 */
function notAProcedure(name, actualType=undefined) {
  return new Error(ErrorType.NOT_A_PROCEDURE, name, /*actualType TODO re-enable */);
}

exports = { argumentTypeError, notAProcedure };