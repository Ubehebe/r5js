/* Copyright 2011-2014 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */


goog.provide('r5js.error');

goog.require('goog.functions');



/**
 * @param {!r5js.Error.Type} type
 * @param {...*} var_args
 * @struct
 * @constructor
 */
r5js.Error = function(type, var_args) {
  /** @const */ this.type = type;
};


/** @enum {string} */
r5js.Error.Type = {
  ARGUMENT_TYPE_ERROR: 'argument type error',
  ILLEGAL_EMPTY_APPLICATION: 'illegal empty application',
  IMMUTABLE_ERROR: 'immutable error',
  INCORRECT_NUM_ARGS: 'incorrect number of args',
  INTERNAL_INTERPRETER_ERROR: 'internal interpreter error',
  IO_ERROR: 'I/O error',
  MACRO_ERROR: 'macro error',
  NOT_A_PROCEDURE_ERROR: 'not a procedure',
  PARSE_ERROR: 'parse error',
  READ_ERROR: 'read error',
  QUASIQUOTE_ERROR: 'quasiquote error',
  SCAN_ERROR: 'scan error',
  TOO_FEW_VARARGS: 'too few varargs',
  TOO_MANY_VARARGS: 'too many varargs',
  UNBOUND_VARIABLE: 'unbound variable',
  UNIMPLEMENTED_OPTION_ERROR: 'unimplemented option'
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
  return new r5js.Error(r5js.Error.Type.UNBOUND_VARIABLE, name);
};


/**
 * @param {string} name The name of the procedure.
 * @param {number} minNumArgs The procedure's minimum number of arguments.
 * @param {number} actualNumArgs The actual number of arguments passed to
 * the procedure.
 * @return {!r5js.Error}
 */
r5js.error.tooFewVarargs = function(name, minNumArgs, actualNumArgs) {
  return new r5js.Error(r5js.Error.Type.TOO_FEW_VARARGS,
      name, minNumArgs, actualNumArgs);
};


/**
 * @param {string} name The name of the procedure.
 * @param {number} maxNumArgs The procedure's maximum number of arguments.
 * @param {number} actualNumArgs The actual number of arguments passed to
 * the procedure.
 * @return {!r5js.Error}
 */
r5js.error.tooManyVarargs = function(name, maxNumArgs, actualNumArgs) {
  return new r5js.Error(r5js.Error.Type.TOO_MANY_VARARGS,
      name, maxNumArgs, actualNumArgs);
};


/**
 * @param {string} name The name of the procedure.
 * @param {number} expectedNumArgs The expected number of arguments.
 * @param {number} actualNumArgs The actual number of arguments.
 * @return {!r5js.Error}
 */
r5js.error.incorrectNumArgs = function(name, expectedNumArgs, actualNumArgs) {
  return new r5js.Error(r5js.Error.Type.INCORRECT_NUM_ARGS,
      name, expectedNumArgs, actualNumArgs);
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
  return new r5js.Error(r5js.Error.Type.ARGUMENT_TYPE_ERROR,
      arg, argIndex, procName, expectedType, actualType);
};


/**
 * @param {string} keyword Keyword of macro.
 * @param {string} msg Error message.
 * @return {!r5js.Error}
 */
r5js.error.macro = function(keyword, msg) {
  return new r5js.Error(r5js.Error.Type.MACRO_ERROR, keyword, msg);
};


/**
 * @param {string} what An error message.
 * @return {!r5js.Error}
 */
r5js.error.unimplementedOption = function(what) {
  return new r5js.Error(
      r5js.Error.Type.UNIMPLEMENTED_OPTION_ERROR, what);
};


/**
 * @param {string} what An error message.
 * @return {!r5js.Error}
 */
r5js.error.quasiquote = function(what) {
  return new r5js.Error(r5js.Error.Type.QUASIQUOTE_ERROR, what);
};


/**
 * @param {*} where Object that caused the empty application.
 * @return {!r5js.Error}
 */
r5js.error.illegalEmptyApplication = function(where) {
  return new r5js.Error(
      r5js.Error.Type.ILLEGAL_EMPTY_APPLICATION, where);
};


/**
 * @param {*} what
 * @return {!r5js.Error}
 */
r5js.error.parse = function(what) {
  return new r5js.Error(r5js.Error.Type.PARSE_ERROR, what);
};


/**
 * @param {!r5js.Token} token
 * @return {!r5js.Error}
 */
r5js.error.read = function(token) {
  return new r5js.Error(r5js.Error.Type.READ_ERROR, token);
};


/**
 * @param {string} name Error message.
 * @param {!r5js.Type} actualType
 * @return {!r5js.Error}
 */
r5js.error.notAProcedure = function(name, actualType) {
  return new r5js.Error(r5js.Error.Type.NOT_A_PROCEDURE_ERROR,
      name, actualType);
};


/**
  * @param {string} what Object that caused the error.
 * @return {!r5js.Error}
 */
r5js.error.immutable = function(what) {
  return new r5js.Error(r5js.Error.Type.IMMUTABLE_ERROR, what);
};


/**
 * @param {string} what An error message.
 * @return {!r5js.Error}
 */
r5js.error.scan = function(what) {
  return new r5js.Error(r5js.Error.Type.SCAN_ERROR, what);
};
