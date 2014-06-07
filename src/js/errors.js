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
goog.provide('r5js.ReadError');
goog.provide('r5js.ScanError');
goog.provide('r5js.TooFewVarargs');
goog.provide('r5js.TooManyVarargs');
goog.provide('r5js.UnboundVariable');
goog.provide('r5js.UnimplementedOptionError');


goog.require('goog.functions');



/** @interface */
r5js.Error = function() {};


/** @override */
r5js.Error.prototype.toString = function() {};


/** @return {string} */
r5js.Error.prototype.getShortName = function() {};


/**
 * @param {!r5js.Error} other
 * @return {boolean}
 */
r5js.Error.prototype.equals = function(other) {};



/**
 * @param {string} name The name of the variable that was supposed to be
 * bound but wasn't.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 */
r5js.UnboundVariable = function(name) {
  this.toString = function() {
    return 'unbound variable ' + name;
  };
};


/** @override */
r5js.UnboundVariable.prototype.getShortName =
    goog.functions.constant('UnboundVariable');


/** @override */
r5js.UnboundVariable.prototype.equals = goog.functions.FALSE;



/**
 * @param {string} name The name of the procedure.
 * @param {number} minNumArgs The procedure's minimum number of arguments.
 * @param {number} actualNumArgs The actual number of arguments passed to
 * the procedure.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 */
r5js.TooFewVarargs = function(name, minNumArgs, actualNumArgs) {
  /** @const @private */ this.name_ = name;
  /** @const @private */ this.minNumArgs_ = minNumArgs;
  /** @const @private */ this.actualNumArgs_ = actualNumArgs;
};


/** @override */
r5js.TooFewVarargs.prototype.toString = function() {
  return this.name_ +
      ': too few args: want >= ' +
      this.minNumArgs_ +
      ', got ' +
      this.actualNumArgs_;
};


/** @override */
r5js.TooFewVarargs.prototype.getShortName =
    goog.functions.constant('TooFewVarargs');


/** @override */
r5js.TooFewVarargs.prototype.equals = function(other) {
  if (!(other instanceof r5js.TooFewVarargs)) {
    return false;
  }
  other = /** @type {!r5js.TooFewVarargs} */ (other);
  return this.name_ === other.name_ &&
      this.minNumArgs_ === other.minNumArgs_ &&
      this.actualNumArgs_ === other.actualNumArgs_;
};



/**
 * @param {string} name The name of the procedure.
 * @param {number} maxNumArgs The procedure's maximum number of arguments.
 * @param {number} actualNumArgs The actual number of arguments passed to
 * the procedure.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 */
r5js.TooManyVarargs = function(name, maxNumArgs, actualNumArgs) {
  /** @const @private */ this.name_ = name;
  /** @const @private */ this.maxNumArgs_ = maxNumArgs;
  /** @const @private */ this.actualNumArgs_ = actualNumArgs;
};


/** @override */
r5js.TooManyVarargs.prototype.toString = function() {
  return this.name_ +
      ': too many args: want <= ' +
      this.maxNumArgs_ +
      ', got ' +
      this.actualNumArgs_;
};


/** @override */
r5js.TooManyVarargs.prototype.getShortName =
    goog.functions.constant('TooManyVarargs');


/** @override */
r5js.TooManyVarargs.prototype.equals = function(other) {
  if (!(other instanceof r5js.TooManyVarargs)) {
    return false;
  }
  other = /** @type {!r5js.TooManyVarargs} */ (other);
  return this.name_ === other.name_ &&
      this.maxNumArgs_ === other.maxNumArgs_ &&
      this.actualNumArgs_ === other.actualNumArgs_;
};



/**
 * @param {string} name The name of the procedure.
 * @param {number} expectedNumArgs The expected number of arguments.
 * @param {number} actualNumArgs The actual number of arguments.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 */
r5js.IncorrectNumArgs = function(name, expectedNumArgs, actualNumArgs) {
  /** @const */ this.name = name;
  /** @const */ this.expectedNumArgs = expectedNumArgs;
  /** @const */ this.actualNumArgs = actualNumArgs;
};


/** @override */
r5js.IncorrectNumArgs.prototype.toString = function() {
  return this.name +
      ': want ' +
      this.expectedNumArgs +
      ' args, got ' +
      this.actualNumArgs;
};


/** @override */
r5js.IncorrectNumArgs.prototype.getShortName =
    goog.functions.constant('IncorrectNumArgs');


/** @override */
r5js.IncorrectNumArgs.prototype.equals = function(other) {
  if (!(other instanceof r5js.IncorrectNumArgs)) {
    return false;
  }
  other = /** @type {!r5js.IncorrectNumArgs} */ (other);
  // TODO bl check the name once lambdas are plumbed
  return this.expectedNumArgs === other.expectedNumArgs &&
      this.actualNumArgs === other.actualNumArgs;
};



/**
 * @param {string} msg An error message.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 */
r5js.InternalInterpreterError = function(msg) {
  this.toString = function() {
    return msg;
  };
};


/** @override */
r5js.InternalInterpreterError.prototype.getShortName =
    goog.functions.constant('InternalInterpreterError');


/** @override */
r5js.InternalInterpreterError.prototype.equals = goog.functions.FALSE;



/**
 * @param {!r5js.runtime.Value} arg The argument.
 * @param {number} argIndex The position of the argument in the argument list
 * (zero-indexed).
 * @param {string} procName The procedure that the interpreter was invoking
 * when this error occurred.
 * @param {!r5js.Type} expectedType The type of the argument
 * that the interpreter expected.
 * @param {!r5js.Type} actualType The actual type of the argument.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 */
r5js.ArgumentTypeError = function(
    arg, argIndex, procName, expectedType, actualType) {
  /** @const @private */ this.arg_ = arg;
  /** @const @private */ this.argIndex_ = argIndex;
  /** @const @private */ this.procName_ = procName;
  /** @const @private */ this.expectedType_ = expectedType;
  /** @const @private */ this.actualType_ = actualType;
};


/** @override */
r5js.ArgumentTypeError.prototype.toString = function() {
  return this.procName_ +
      ': wrong type for argument ' +
      (this.argIndex_ + 1) + // one-indexed for human readability
      ': want ' +
      this.expectedType_ +
      ', got ' +
      this.actualType_;
};


/** @override */
r5js.ArgumentTypeError.prototype.getShortName =
    goog.functions.constant('ArgumentTypeError');


/** @override */
r5js.ArgumentTypeError.prototype.equals = function(other) {
  if (!(other instanceof r5js.ArgumentTypeError)) {
    return false;
  }
  other = /** @type {!r5js.ArgumentTypeError} */ (other);
  /* TODO bl: it would be nice to test this.arg_ === other.arg_ too,
    but to do this right would require making the interpreter available here. */
  return this.argIndex_ === other.argIndex_ &&
      this.procName_ === other.procName_ &&
      this.expectedType_ === other.expectedType_ &&
      this.actualType_ === other.actualType_;
};



/**
 * @param {string} keyword Keyword of macro.
 * @param {string} msg Error message.
 * @implements {r5js.Error}
 * @struct
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


/** @override */
r5js.MacroError.prototype.getShortName =
    goog.functions.constant('r5js.MacroError');


/** @override */
r5js.MacroError.prototype.equals = goog.functions.FALSE;



/**
 * @param {string} what An error message.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 */
r5js.UnimplementedOptionError = function(what) {
  /** @const @private */ this.what_ = what;
};


/** @override */
r5js.UnimplementedOptionError.prototype.toString = function() {
  return 'unimplemented optional procedure: ' + this.what_;
};


/** @override */
r5js.UnimplementedOptionError.prototype.getShortName =
    goog.functions.constant('UnimplementedOptionError');


/** @override */
r5js.UnimplementedOptionError.prototype.equals = function(other) {
  return other instanceof r5js.UnimplementedOptionError;
};



/**
 * @param {string} what An error message.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 */
r5js.IOError = function(what) {
  this.toString = function() {
    return 'IO error: ' + what;
  };
};


/** @override */
r5js.IOError.prototype.getShortName = goog.functions.constant('IOError');


/** @override */
r5js.IOError.prototype.equals = goog.functions.FALSE;



/**
 * @param {string} what An error message.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 * TODO bl: There is only one caller of this exception. Can that caller use
 * something else?
 */
r5js.QuasiquoteError = function(what) {
  this.toString = function() {
    return 'quasiquote error: ' + what;
  };
};


/** @override */
r5js.QuasiquoteError.prototype.getShortName =
    goog.functions.constant('QuasiquoteError');


/** @override */
r5js.QuasiquoteError.prototype.equals = goog.functions.FALSE;



/**
 * @param {*} where Object that caused the empty application.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 * TODO bl: narrow the type of the parameter. Can it be string?
 */
r5js.IllegalEmptyApplication = function(where) {
  this.toString = function() {
    return 'illegal empty application in ' + where;
  };
};


/** @override */
r5js.IllegalEmptyApplication.prototype.getShortName =
    goog.functions.constant('IllegalEmptyApplication');


/** @override */
r5js.IllegalEmptyApplication.prototype.equals = goog.functions.FALSE;



/**
 * @param {*} what
 * @implements {r5js.Error}
 * @struct
 * @constructor
 * TODO bl: Narrow the type of the parameter.
 */
r5js.ParseError = function(what) {
  this.toString = function() {
    return 'parse error on ' + what;
  };
};


/** @override */
r5js.ParseError.prototype.getShortName = goog.functions.constant('ParseError');


/** @override */
r5js.ParseError.prototype.equals = goog.functions.FALSE;



/**
 * @param {!r5js.Token} token
 * @implements {r5js.Error}
 * @struct
 * @constructor
 */
r5js.ReadError = function(token) {
  /** @const @private */ this.token_ = token;
};


/** @override */
r5js.ReadError.prototype.toString = function() {
  return 'read error: ' + this.token_;
};


/** @override */
r5js.ReadError.prototype.getShortName = goog.functions.constant('ReadError');


/** @override */
r5js.ReadError.prototype.equals = function(other) {
  if (!(other instanceof r5js.ReadError)) {
    return false;
  }
  return this.token_ === other.token_;
};



/**
 * @param {string} what Error message.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 */
r5js.EvalError = function(what) {
  this.toString = function() {
    return 'evaluation error: ' + what;
  };
};


/** @override */
r5js.EvalError.prototype.getShortName = goog.functions.constant('EvalError');


/** @override */
r5js.EvalError.prototype.equals = goog.functions.FALSE;



/**
 * @param {string} what Object that caused the error.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 */
r5js.ImmutableError = function(what) {
  this.toString = function() {
    return 'cannot mutate immutable object: ' + what;
  };
};


/** @override */
r5js.ImmutableError.prototype.getShortName =
    goog.functions.constant('ImmutableError');


/** @override */
r5js.ImmutableError.prototype.equals = goog.functions.FALSE;



/**
 * @param {string} what An error message.
 * @implements {r5js.Error}
 * @struct
 * @constructor
 */
r5js.ScanError = function(what) {
  this.toString = function() {
    return 'scan error on ' + what;
  };
};


/** @override */
r5js.ScanError.prototype.equals = goog.functions.FALSE;


/** @override */
r5js.ScanError.prototype.getShortName = goog.functions.constant('ScanError');
