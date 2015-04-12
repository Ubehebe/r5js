goog.provide('r5js.Error');
goog.provide('r5js.error');

goog.require('r5js.Token');
goog.require('r5js.Type');


r5js.Error = class {
    /**
     * @param {!r5js.Error.Type} type
     * @param {string} msg Human-readable error message.
     * @param {...*} var_args
     */
    constructor(type, msg, var_args) {
        /** @const */ this.type = type;
        /** @const */ this.msg = msg;
    }
};


/** @enum {string} */
r5js.Error.Type = {
  ARGUMENT_TYPE_ERROR: 'argument type error',
  ILLEGAL_EMPTY_APPLICATION: 'illegal empty application',
  IMMUTABLE: 'immutable error',
  INCORRECT_NUM_ARGS: 'incorrect number of args',
  INTERNAL_INTERPRETER_ERROR: 'internal interpreter error',
  IO: 'I/O error',
  MACRO: 'macro error',
  NOT_A_PROCEDURE: 'not a procedure',
  PARSE: 'parse error',
  READ: 'read error',
  QUASIQUOTE: 'quasiquote error',
  SCAN: 'scan error',
  TOO_FEW_VARARGS: 'too few varargs',
  TOO_MANY_VARARGS: 'too many varargs',
  UNBOUND_VARIABLE: 'unbound variable',
  UNIMPLEMENTED_OPTION: 'unimplemented option'
};


/**
 * @param {!r5js.Error} e1
 * @param {!r5js.Error} e2
 * @return {boolean}
 */
r5js.error.equals = function(e1, e2) {
  return e1.type === e2.type; // TODO bl improve
};


/**
 * @param {string} name The name of the variable that was supposed to be bound
 * but wasn't.
 * @return {!r5js.Error}
 */
r5js.error.unboundVariable = function(name) {
  return new r5js.Error(
      r5js.Error.Type.UNBOUND_VARIABLE,
      name + ' is not defined');
};


/**
 * @param {string} name The name of the procedure.
 * @param {number} minNumArgs The procedure's minimum number of arguments.
 * @param {number} actualNumArgs The actual number of arguments passed to
 * the procedure.
 * @return {!r5js.Error}
 */
r5js.error.tooFewVarargs = function(name, minNumArgs, actualNumArgs) {
  return new r5js.Error(
      r5js.Error.Type.TOO_FEW_VARARGS,
      name + ': want >= ' + minNumArgs + ' args, got ' + actualNumArgs);
};


/**
 * @param {string} name The name of the procedure.
 * @param {number} maxNumArgs The procedure's maximum number of arguments.
 * @param {number} actualNumArgs The actual number of arguments passed to
 * the procedure.
 * @return {!r5js.Error}
 */
r5js.error.tooManyVarargs = function(name, maxNumArgs, actualNumArgs) {
  return new r5js.Error(
      r5js.Error.Type.TOO_MANY_VARARGS,
      name + ': want <= ' + maxNumArgs + ' args, got ' + actualNumArgs);
};


/**
 * @param {string} name The name of the procedure.
 * @param {number} expectedNumArgs The expected number of arguments.
 * @param {number} actualNumArgs The actual number of arguments.
 * @return {!r5js.Error}
 */
r5js.error.incorrectNumArgs = function(name, expectedNumArgs, actualNumArgs) {
  return new r5js.Error(
      r5js.Error.Type.INCORRECT_NUM_ARGS,
      name + ': want ' + expectedNumArgs + ' args, got ' + actualNumArgs);
};


/**
 * @param {string} msg An error message.
 * @return {!r5js.Error}
 */
r5js.error.internalInterpreterError = function(msg) {
  return new r5js.Error(r5js.Error.Type.INTERNAL_INTERPRETER_ERROR, msg);
};


/**
 * @param {!r5js.runtime.Value} arg The argument.
 * @param {number} argIndex The position of the argument in the argument list
 * (zero-indexed).
 * @param {string} procName The procedure that the interpreter was invoking
 * when this error occurred.
 * @param {!r5js.Type} expectedType The type of the argument
 * that the interpreter expected.
 * @param {!r5js.Type} actualType The actual type of the argument.
 * @return {!r5js.Error}
 */
r5js.error.argumentTypeError = function(
    arg, argIndex, procName, expectedType, actualType) {
  return new r5js.Error(
      r5js.Error.Type.ARGUMENT_TYPE_ERROR,
      procName + ': argument ' + argIndex + ': want ' +
          expectedType + ', got ' + actualType);
};


/**
 * @param {string} keyword Keyword of macro.
 * @param {string} msg Error message.
 * @return {!r5js.Error}
 */
r5js.error.macro = function(keyword, msg) {
  return new r5js.Error(
      r5js.Error.Type.MACRO,
      'macro ' + keyword + ': ' + msg);
};


/**
 * @param {string} what An error message.
 * @return {!r5js.Error}
 */
r5js.error.unimplementedOption = function(what) {
  return new r5js.Error(
      r5js.Error.Type.UNIMPLEMENTED_OPTION,
      'unimplemented: ' + what);
};


/**
 * @param {string} what An error message.
 * @return {!r5js.Error}
 */
r5js.error.quasiquote = function(what) {
  return new r5js.Error(r5js.Error.Type.QUASIQUOTE, what);
};


/**
 * @param {string} what Object that caused the empty application.
 * @return {!r5js.Error}
 */
r5js.error.illegalEmptyApplication = function(what) {
  return new r5js.Error(
      r5js.Error.Type.ILLEGAL_EMPTY_APPLICATION, what);
};


/**
 * @param {*} what
 * @return {!r5js.Error}
 */
r5js.error.parse = function(what) {
  return new r5js.Error(r5js.Error.Type.PARSE, 'parse error: ' + what);
};


/**
 * @param {!r5js.Token} token
 * @return {!r5js.Error}
 */
r5js.error.read = function(token) {
  return new r5js.Error(r5js.Error.Type.READ, 'read error: ' + token);
};


/**
 * @param {string} name Error message.
 * @param {!r5js.Type} actualType
 * @return {!r5js.Error}
 */
r5js.error.notAProcedure = function(name, actualType) {
  return new r5js.Error(r5js.Error.Type.NOT_A_PROCEDURE,
      name, actualType);
};


/**
  * @param {string} what Object that caused the error.
 * @return {!r5js.Error}
 */
r5js.error.immutable = function(what) {
  return new r5js.Error(r5js.Error.Type.IMMUTABLE, what);
};


/**
 * @param {string} what An error message.
 * @return {!r5js.Error}
 */
r5js.error.scan = function(what) {
  return new r5js.Error(r5js.Error.Type.SCAN, 'scan error: ' + what);
};
