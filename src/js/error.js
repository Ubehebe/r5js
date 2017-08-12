goog.module('r5js.Error');
goog.module.declareLegacyNamespace();

const Token = goog.require('r5js.Token');
const Type = goog.require('r5js.Type');
const Value = goog.require('r5js.runtime.Value');

class Error {
    /**
     * @param {!Error.Type} type
     * @param {string} msg Human-readable error message.
     * @param {...*} var_args
     */
    constructor(type, msg, var_args) {
        /** @const */ this.type = type;
        /** @const */ this.msg = msg;
    }

    /**
     * @param {!Error} other
     * @return {boolean}
     */
    equals(other) {
        return this.type === other.type; // TODO bl improve
    }

    /**
     * @param {string} name The name of the variable that was supposed to be bound but wasn't.
     * @return {!Error}
     */
    static unboundVariable(name) {
        return new Error(
            Error.Type.UNBOUND_VARIABLE,
            name + ' is not defined');
    }

    /**
     * @param {string} name The name of the procedure.
     * @param {number} minNumArgs The procedure's minimum number of arguments.
     * @param {number} actualNumArgs The actual number of arguments passed to
     * the procedure.
     * @return {!Error}
     */
    static tooFewVarargs(name, minNumArgs, actualNumArgs) {
        return new Error(
            Error.Type.TOO_FEW_VARARGS,
            name + ': want >= ' + minNumArgs + ' args, got ' + actualNumArgs);
    }

    /**
     * @param {string} name The name of the procedure.
     * @param {number} maxNumArgs The procedure's maximum number of arguments.
     * @param {number} actualNumArgs The actual number of arguments passed to the procedure.
     * @return {!Error}
     */
    static tooManyVarargs(name, maxNumArgs, actualNumArgs) {
        return new Error(
            Error.Type.TOO_MANY_VARARGS,
            name + ': want <= ' + maxNumArgs + ' args, got ' + actualNumArgs);
    }

    /**
     * @param {string} name The name of the procedure.
     * @param {number} expectedNumArgs The expected number of arguments.
     * @param {number} actualNumArgs The actual number of arguments.
     * @return {!Error}
     */
    static incorrectNumArgs(name, expectedNumArgs, actualNumArgs) {
        return new Error(
            Error.Type.INCORRECT_NUM_ARGS,
            name + ': want ' + expectedNumArgs + ' args, got ' + actualNumArgs);
    }

    /**
     * @param {string} msg An error message.
     * @return {!Error}
     */
    static internalInterpreterError(msg) {
        return new Error(Error.Type.INTERNAL_INTERPRETER_ERROR, msg);
    }

    /**
     * @param {!Value} arg The argument.
     * @param {number} argIndex The position of the argument in the argument list (zero-indexed).
     * @param {string} procName The procedure that the interpreter was invoking when this error
     * occurred.
     * @param {!Type} expectedType The type of the argument that the interpreter expected.
     * @param {!Type} actualType The actual type of the argument.
     * @return {!Error}
     */
    static argumentTypeError(arg, argIndex, procName, expectedType, actualType) {
        return new Error(
            Error.Type.ARGUMENT_TYPE_ERROR,
            procName + ': argument ' + argIndex + ': want ' +
            expectedType + ', got ' + actualType);
    }

    /**
     * @param {string} keyword Keyword of macro.
     * @param {string} msg Error message.
     * @return {!Error}
     */
    static macro(keyword, msg) {
        return new Error(
            Error.Type.MACRO,
            'macro ' + keyword + ': ' + msg);
    }

    /**
     * @param {string} what An error message.
     * @return {!Error}
     */
    static unimplementedOption(what) {
        return new Error(
            Error.Type.UNIMPLEMENTED_OPTION,
            'unimplemented: ' + what);
    }

    /**
     * @param {string} what An error message.
     * @return {!Error}
     */
    static quasiquote(what) {
        return new Error(Error.Type.QUASIQUOTE, what);
    }

    /**
     * @param {string} what Object that caused the empty application.
     * @return {!Error}
     */
    static illegalEmptyApplication(what) {
        return new Error(Error.Type.ILLEGAL_EMPTY_APPLICATION, what);
    }

    /**
     * @param {*} what
     * @return {!r5js.Error}
     */
    static parse(what) {
        return new Error(Error.Type.PARSE, 'parse error: ' + what);
    }

    /**
     * @param {!Token} token
     * @return {!Error}
     */
    static read(token) {
        return new Error(Error.Type.READ, 'read error: ' + token);
    }

    /**
     * @param {string} name Error message.
     * @param {!Type=} opt_actualType
     * @return {!Error}
     */
    static notAProcedure(name, opt_actualType) {
        return new Error(Error.Type.NOT_A_PROCEDURE, name, opt_actualType);
    }

    /**
     * @param {string} what Object that caused the error.
     * @return {!Error}
     */
    static immutable(what) {
        return new Error(Error.Type.IMMUTABLE, what);
    }

    /**
     * @param {string} what An error message.
     * @return {!Error}
     */
    static scan(what) {
        return new Error(Error.Type.SCAN, 'scan error: ' + what);
    }
}

/** @enum {string} */
Error.Type = {
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

exports = Error;
