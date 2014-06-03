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


/**
 * @fileoverview Error classes. The constructor names are exported
 * so they are still informative when thrown from compiled code.
 */

goog.provide('r5js.ArgumentTypeError');
goog.provide('r5js.EvalError');
goog.provide('r5js.FFIError');
goog.provide('r5js.GeneralSyntaxError');
goog.provide('r5js.IOError');
goog.provide('r5js.IllegalEmptyApplication');
goog.provide('r5js.ImmutableError');
goog.provide('r5js.IncorrectNumArgs');
goog.provide('r5js.InternalInterpreterError');
goog.provide('r5js.MacroError');
goog.provide('r5js.ParseError');
goog.provide('r5js.PrimitiveProcedureError');
goog.provide('r5js.QuasiquoteError');
goog.provide('r5js.ScanError');
goog.provide('r5js.TooFewArgs');
goog.provide('r5js.TooManyArgs');
goog.provide('r5js.UnboundVariable');
goog.provide('r5js.UnimplementedOptionError');



/**
 * @param {string} name The name of the variable that was supposed to be
 * bound but wasn't.
 * @constructor
 */
r5js.UnboundVariable = function(name) {
  this.toString = function() {
    return 'unbound variable ' + name;
  };
};
goog.exportSymbol('r5js.UnboundVariable', r5js.UnboundVariable);



/**
 * @param {string} name The name of the procedure.
 * @param {number} minNumArgs The procedure's minimum number of arguments.
 * @param {number} actualNumArgs The actual number of arguments passed to
 * the procedure.
 * @constructor
 */
r5js.TooFewArgs = function(name, minNumArgs, actualNumArgs) {
  this.toString = function() {
    return 'The procedure ' +
        name +
        ' has been called with ' +
        actualNumArgs +
        ' argument' +
        (actualNumArgs === 1 ? '' : 's') +
        '; it requires at least ' +
        minNumArgs +
        ' argument' +
        (minNumArgs === 1 ? '' : 's');
  };
};
goog.exportSymbol('r5js.TooFewArgs', r5js.TooFewArgs);



/**
 * @param {string} name The name of the procedure.
 * @param {number} maxNumArgs The procedure's maximum number of arguments.
 * @param {number} actualNumArgs The actual number of arguments passed to
 * the procedure.
 * @constructor
 */
r5js.TooManyArgs = function(name, maxNumArgs, actualNumArgs) {
  this.toString = function() {
    return 'The procedure ' +
        name +
        ' has been called with ' +
        actualNumArgs +
        ' argument' +
        (actualNumArgs === 1 ? '' : 's') +
        '; it requires at most ' +
        maxNumArgs +
        ' argument' +
        (maxNumArgs === 1 ? '' : 's');
  };
};
goog.exportSymbol('r5js.TooManyArgs', r5js.TooManyArgs);



/**
 * @param {string} name The name of the procedure.
 * @param {number} expectedNumArgs The expected number of arguments.
 * @param {number} actualNumArgs The actual number of arguments.
 * @constructor
 */
r5js.IncorrectNumArgs = function(name, expectedNumArgs, actualNumArgs) {
  this.toString = function() {
    return 'The procedure ' +
        name +
        ' has been called with ' +
        actualNumArgs +
        ' argument' +
        (actualNumArgs === 1 ? '' : 's') +
        '; it requires exactly ' +
        expectedNumArgs +
        ' argument' +
        (expectedNumArgs === 1 ? '' : 's');
  };
};
goog.exportSymbol('r5js.IncorrectNumArgs', r5js.IncorrectNumArgs);



/**
 * @param {string} msg An error message.
 * @constructor
 */
r5js.InternalInterpreterError = function(msg) {
  this.toString = function() {
    return msg;
  };
};
goog.exportSymbol(
    'r5js.InternalInterpreterError', r5js.InternalInterpreterError);



/**
 * @param {string} message An error message.
 * @constructor
 * TODO bl: consider renaming to RuntimeError.
 */
r5js.PrimitiveProcedureError = function(message) {
  this.toString = function() { return message; };
};
goog.exportSymbol('r5js.PrimitiveProcedureError', r5js.PrimitiveProcedureError);



/**
 * @param {*} argument The argument.
 * @param {number} which The position of the argument in the argument list
 * (zero-indexed).
 * @param {string} procName The procedure that the interpreter was invoking
 * when this error occurred.
 * @param {!r5js.Type} expectedType The type of the argument
 * that the interpreter expected.
 * @constructor
 */
r5js.ArgumentTypeError = function(argument, which, procName, expectedType) {
  this.toString = function() {
    return 'The object ' +
        argument.toString() +
        ', passed as argument ' +
        which +
        ' to ' +
        procName +
        ', is not of the correct type ' +
        expectedType.toString();
  };
};
goog.exportSymbol('r5js.ArgumentTypeError', r5js.ArgumentTypeError);



/**
 * @param {string} keyword Keyword of macro.
 * @param {string} msg Error message.
 * @constructor
 * TODO bl: This should accept a macro object to simplify call sites.
 */
r5js.MacroError = function(keyword, msg) {
  this.toString = function() {
    return 'Error in macro ' +
        keyword +
        ': ' +
        msg;
  };
};
goog.exportSymbol('r5js.MacroError', r5js.MacroError);



/**
 * @param {string} what An error message.
 * @constructor
 */
r5js.UnimplementedOptionError = function(what) {
  this.toString = function() {
    return 'Sorry, ' +
        what +
        ' is optional according to R5RS and unimplemented';
  };
};
goog.exportSymbol(
    'r5js.UnimplementedOptionError', r5js.UnimplementedOptionError);



/**
 * @param {*} what The object that caused the syntax error.
 * @constructor
 * TODO bl: narrow the type of the parameter.
 * TODO bl: Consider eliminating. It's vague.
 */
r5js.GeneralSyntaxError = function(what) {
  this.toString = function() {
    return 'bad syntax in ' + what;
  };
};
goog.exportSymbol('r5js.GeneralSyntaxError', r5js.GeneralSyntaxError);



/**
 * @param {string} what An error message.
 * @constructor
 */
r5js.IOError = function(what) {
  this.toString = function() {
    return 'IO error: ' + what;
  };
};
goog.exportSymbol('r5js.IOError', r5js.IOError);



/**
 * @param {string} what An error message.
 * @constructor
 * TODO bl: There is only one caller of this exception. Can that caller use
 * something else?
 */
r5js.QuasiquoteError = function(what) {
  this.toString = function() {
    return 'quasiquote error: ' + what;
  };
};
goog.exportSymbol('r5js.QuasiquoteError', r5js.QuasiquoteError);



/**
 * @param {*} where Object that caused the empty application.
 * @constructor
 * TODO bl: narrow the type of the parameter. Can it be string?
 */
r5js.IllegalEmptyApplication = function(where) {
  this.toString = function() {
    return 'illegal empty application in ' + where;
  };
};
goog.exportSymbol('r5js.IllegalEmptyApplication', r5js.IllegalEmptyApplication);



/**
 * @param {*} what
 * @constructor
 * TODO bl: Narrow the type of the parameter.
 */
r5js.ParseError = function(what) {
  this.toString = function() {
    return 'parse error on ' + what;
  };
};
goog.exportSymbol('r5js.ParseError', r5js.ParseError);



/**
 * @param {string} what Error message.
 * @constructor
 */
r5js.EvalError = function(what) {
  this.toString = function() {
    return 'evaluation error: ' + what;
  };
};
goog.exportSymbol('r5js.EvalError', r5js.EvalError);



/**
 * @param {string} what Object that caused the error.
 * @constructor
 */
r5js.ImmutableError = function(what) {
  this.toString = function() {
    return 'cannot mutate immutable object: ' + what;
  };
};
goog.exportSymbol('r5js.ImmutableError', r5js.ImmutableError);



/**
 * @param {string} what An error message.
 * @constructor
 */
r5js.ScanError = function(what) {
  this.toString = function() {
    return 'scan error on ' + what;
  };
};
goog.exportSymbol('r5js.ScanError', r5js.ScanError);
