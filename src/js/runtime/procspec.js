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

goog.provide('r5js.procspec');


goog.require('goog.array');
goog.require('r5js.Procedure');
goog.require('r5js.datumutil');
goog.require('r5js.error');



/**
 * @interface
 * @private
 */
r5js.procspec.NumArgChecker_ = function() {};


/**
 * @param {number} numArgs
 * @param {string} nameToShowInErrorMessage
 */
r5js.procspec.NumArgChecker_.prototype.checkNumArgs = function(
    numArgs, nameToShowInErrorMessage) {};



/**
 * @param {number} numArgs
 * @implements {r5js.procspec.NumArgChecker_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.Exactly_ = function(numArgs) {
  /** @const @private {number} */
  this.numArgs_ = numArgs;
};


/** @override */
r5js.procspec.Exactly_.prototype.checkNumArgs = function(
    numArgs, nameToShowInErrorMessage) {
  if (numArgs !== this.numArgs_) {
    throw r5js.error.incorrectNumArgs(
        nameToShowInErrorMessage, this.numArgs_, numArgs);
  }
};



/**
 * @param {number} min
 * @implements {r5js.procspec.NumArgChecker_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.AtLeast_ = function(min) {
  this.min_ = min;
};


/** @override */
r5js.procspec.AtLeast_.prototype.checkNumArgs = function(
    numArgs, nameToShowInErrorMessage) {
  if (numArgs < this.min_) {
    throw r5js.error.tooFewVarargs(
        nameToShowInErrorMessage, this.min_, numArgs);
  }
};



/**
 * @param {number} minArgs
 * @param {number} maxArgs
 * @implements {r5js.procspec.NumArgChecker_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.Between_ = function(minArgs, maxArgs) {
  /** @const @private {number} */
  this.minArgs_ = minArgs;

  /** @const @private {number} */
  this.maxArgs_ = maxArgs;
};


/** @override */
r5js.procspec.Between_.prototype.checkNumArgs = function(
    numArgs, nameToShowInErrorMessage) {
  if (numArgs < this.minArgs_) {
    throw r5js.error.tooFewVarargs(
        nameToShowInErrorMessage, this.minArgs_, numArgs);
  }
  if (numArgs > this.maxArgs_) {
    throw r5js.error.tooManyVarargs(
        nameToShowInErrorMessage, this.maxArgs_, numArgs);
  }
};


/** @const @private {!r5js.procspec.NumArgChecker_} */
r5js.procspec.EXACTLY_1_ARG_ = new r5js.procspec.Exactly_(1);


/** @const @private {!r5js.procspec.NumArgChecker_} */
r5js.procspec.EXACTLY_2_ARGS_ = new r5js.procspec.Exactly_(2);


/** @const @private {!r5js.procspec.NumArgChecker_} */
r5js.procspec.EXACTLY_3_ARGS_ = new r5js.procspec.Exactly_(3);


/** @const @private {!r5js.procspec.NumArgChecker_} */
r5js.procspec.ANY_NUMBER_OF_ARGS_ = new r5js.procspec.AtLeast_(0);


/** @const @private {!r5js.procspec.NumArgChecker_} */
r5js.procspec.AT_LEAST_1_ARG_ = new r5js.procspec.AtLeast_(1);



/**
 * @interface
 * @private
 */
r5js.procspec.ArgumentTypeCheckerAndUnwrapper_ = function() {};


/**
 * @param {!goog.array.ArrayLike} args
 * @param {string} nameToShowInErrorMessage
 * @return {!goog.array.ArrayLike}
 */
r5js.procspec.ArgumentTypeCheckerAndUnwrapper_.prototype.checkAndUnwrapArgs =
    function(args, nameToShowInErrorMessage) {};



/**
 * @param {!Array<!r5js.Type>} argtypes
 * @implements {r5js.procspec.ArgumentTypeCheckerAndUnwrapper_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.ArgumentTypeCheckerAndUnwrapperImpl_ = function(argtypes) {
  /** @const @private {!Array<!r5js.Type>} */
  this.argtypes_ = argtypes;
};


/**
 * @override
 * @suppress {accessControls|checkTypes} for r5js.PrimitiveProcedures_
 */
r5js.procspec.ArgumentTypeCheckerAndUnwrapperImpl_.prototype.
    checkAndUnwrapArgs = function(args, nameToShowInErrorMessage) {
  var unwrappedArgs = [];
  for (var i = 0; i < this.argtypes_.length; ++i) {
    var arg = args[i];
    var expectedType = this.argtypes_[i];
    if (!r5js.PrimitiveProcedures.registry_[expectedType + '?'].fn_.call(
        null, arg)) {
      var actualType = r5js.PrimitiveProcedures.getActualType_(arg);
      throw r5js.error.argumentTypeError(
          arg, i, nameToShowInErrorMessage, expectedType, actualType);
    }
    unwrappedArgs.push(arg instanceof r5js.Datum ? arg.unwrap() : arg);
  }
  return unwrappedArgs;
};



/**
 * @implements {r5js.procspec.ArgumentTypeCheckerAndUnwrapper_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.NoTypeChecking_ = function() {};


/** @override */
r5js.procspec.NoTypeChecking_.prototype.checkAndUnwrapArgs = function(
    args, nameToShowInErrorMessage) {
  return args;
};



/**
 * @param {!r5js.Type} type
 * @implements {r5js.procspec.ArgumentTypeCheckerAndUnwrapper_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.AllArgsOfType_ = function(type) {
  /** @const @private {!r5js.Type} */
  this.type_ = type;
};


/** @const @private {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} */
r5js.procspec.NO_TYPE_RESTRICTIONS_ = new r5js.procspec.NoTypeChecking_();


/**
 * @override
 * @suppress {accessControls} TODO bl
 */
r5js.procspec.AllArgsOfType_.prototype.checkAndUnwrapArgs = function(
    args, nameToShowInErrorMessage) {
  var argtype = this.type_;
  return goog.array.map(args, function(arg, i) {
    if (!(/** @type {!r5js.procspec.PrimitiveProcedure_} */ (
        r5js.PrimitiveProcedures.registry_[argtype + '?'])).fn_.call(
            null, arg)) {
      throw r5js.error.argumentTypeError(
          arg, i, nameToShowInErrorMessage, argtype,
          r5js.PrimitiveProcedures.getActualType_(arg));
    }
    return arg instanceof r5js.Datum ? arg.unwrap() : arg;
  });
};



/**
 * @implements {r5js.procspec.ArgumentTypeCheckerAndUnwrapper_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.JustUnwrapArgs_ = function() {};


/** @override */
r5js.procspec.JustUnwrapArgs_.prototype.checkAndUnwrapArgs = function(
    args, nameToShowInErrorMessage) {
  return args;
};


/** @const @private {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} */
r5js.procspec.JUST_UNWRAP_ARGS_ = new r5js.procspec.JustUnwrapArgs_();



/**
 * @param {!Function} fn TODO bl narrow type?
 * @param {!r5js.procspec.NumArgChecker_} numArgChecker
 * @param {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} typeChecker
 * @extends {r5js.Procedure}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.PrimitiveProcedure_ = function(
    fn, numArgChecker, typeChecker) {
  r5js.procspec.PrimitiveProcedure_.base(this, 'constructor');
  /** @const @private {function(!r5js.Datum):?} */
  this.fn_ = fn;

  /** @const @private {!r5js.procspec.NumArgChecker_} */
  this.numArgChecker_ = numArgChecker;

  /** @const @private {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} */
  this.typeChecker_ = typeChecker;

  /** @private {string} */
  this.debugName_ = '';
};
goog.inherits(r5js.procspec.PrimitiveProcedure_, r5js.Procedure);


/**
 * Procedures have no deep need to know their names, as they are only bindings
 * and can change: (set! car cdr). This method exists only to increase
 * the usefulness of error messages thrown from primitive procedures.
 * @param {string} name
 */
r5js.procspec.PrimitiveProcedure_.prototype.setDebugName = function(name) {
  this.debugName_ = name;
};


/** @return {string} */
r5js.procspec.PrimitiveProcedure_.prototype.getDebugName = function() {
  return this.debugName_;
};


/**
 * @param {!goog.array.ArrayLike} userArgs
 * @param {!r5js.ProcCallLike} procCallLike
 * @param {!r5js.TrampolineHelper} trampolineHelper
 * @protected
 */
r5js.procspec.PrimitiveProcedure_.prototype.call = function(
    userArgs, procCallLike, trampolineHelper) {
  this.numArgChecker_.checkNumArgs(userArgs.length, this.debugName_);
  var unwrappedArgs = this.typeChecker_.checkAndUnwrapArgs(
      userArgs, this.debugName_);
  var ans = this.fn_.apply(null, unwrappedArgs);
  procCallLike.bindResult(ans);
  trampolineHelper.setValue(ans);
  var nextContinuable = procCallLike.getNext();
  if (nextContinuable) {
    trampolineHelper.setNext(nextContinuable);
  }
};


/**
 * Primitive procedure, represented by JavaScript function:
 * (+ x y [ans ...]). We perform the action ("+"), bind the
 * result to the continuation's result name ("ans"), and advance
 * to the next continuable ("...").
 * @override
 */
r5js.procspec.PrimitiveProcedure_.prototype.evaluate = function(
    args, procCallLike, trampolineHelper, env) {
  args = args.map(/** @type {!Function} */ (
      r5js.datumutil.wrapValue));
  // todo bl document why we're doing this...
  for (var i = 0; i < args.length; ++i) {
    if (args[i] instanceof r5js.Ref) {
      args[i] = (/** @type {!r5js.Ref} */ (args[i])).deref();
    }
  }
  this.call(args, procCallLike, trampolineHelper);
};



/**
 * @param {!Function} fn
 * @param {!r5js.procspec.NumArgChecker_} numArgChecker
 * @param {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} typeChecker
 * @extends {r5js.procspec.PrimitiveProcedure_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.NeedsCurrentPorts_ = function(fn, numArgChecker, typeChecker) {
  r5js.procspec.NeedsCurrentPorts_.base(
      this, 'constructor', fn, numArgChecker, typeChecker);
};
goog.inherits(
    r5js.procspec.NeedsCurrentPorts_, r5js.procspec.PrimitiveProcedure_);


/** @override */
r5js.procspec.NeedsCurrentPorts_.prototype.call = function(
    userArgs, procCallLike, trampolineHelper) {
  this.numArgChecker_.checkNumArgs(userArgs.length, this.debugName_);
  var unwrappedArgs = this.typeChecker_.checkAndUnwrapArgs(
      userArgs, this.debugName_);
  var args = goog.array.concat(
      trampolineHelper.getInputPort(),
      trampolineHelper.getOutputPort(),
      goog.array.toArray(unwrappedArgs));
  var ans = this.fn_.apply(null, args);
  procCallLike.bindResult(ans);
  trampolineHelper.setValue(ans);
  var nextContinuable = procCallLike.getNext();
  if (nextContinuable) {
    trampolineHelper.setNext(nextContinuable);
  }
};



/**
 * @param {!Function} fn
 * @param {!r5js.procspec.NumArgChecker_} numArgChecker
 * @param {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} typeChecker
 * @extends {r5js.procspec.PrimitiveProcedure_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.HasSpecialEvalLogic_ = function(fn, numArgChecker, typeChecker) {
  r5js.procspec.HasSpecialEvalLogic_.base(
      this, 'constructor', fn, numArgChecker, typeChecker);
};
goog.inherits(
    r5js.procspec.HasSpecialEvalLogic_, r5js.procspec.PrimitiveProcedure_);


/** @override */
r5js.procspec.HasSpecialEvalLogic_.prototype.call = function(
    userArgs, procCallLike, trampolineHelper) {
  this.numArgChecker_.checkNumArgs(userArgs.length, this.debugName_);
  var unwrappedArgs = this.typeChecker_.checkAndUnwrapArgs(
      userArgs, this.debugName_);
  var args = goog.array.concat(
      goog.array.toArray(unwrappedArgs),
      procCallLike, trampolineHelper);
  this.fn_.apply(null, args);
};


/**
 * @param {function(T):?} fn
 * @param {!r5js.Type=} opt_argtype
 * @return {!r5js.procspec.PrimitiveProcedure_}
 * @template T
 * TODO bl: make the template type mean something
 */
r5js.procspec.unary = function(fn, opt_argtype) {
  return new r5js.procspec.PrimitiveProcedure_(
      fn, r5js.procspec.EXACTLY_1_ARG_,
      goog.isDef(opt_argtype) ?
      new r5js.procspec.ArgumentTypeCheckerAndUnwrapperImpl_([opt_argtype]) :
      r5js.procspec.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(T1, T2):?} fn
 * @param {!r5js.Type=} opt_argtype1
 * @param {!r5js.Type=} opt_argtype2
 * @return {!r5js.procspec.PrimitiveProcedure_}
 * @template T1,T2
 * TODO bl: make the template types mean something
 */
r5js.procspec.binary = function(fn, opt_argtype1, opt_argtype2) {
  var argtypes = [];
  if (goog.isDef(opt_argtype1)) {
    argtypes.push(opt_argtype1);
  }
  if (goog.isDef(opt_argtype2)) {
    argtypes.push(opt_argtype2);
  }
  var typeChecker = argtypes.length === 0 ?
      r5js.procspec.NO_TYPE_RESTRICTIONS_ :
      new r5js.procspec.ArgumentTypeCheckerAndUnwrapperImpl_(argtypes);
  return new r5js.procspec.PrimitiveProcedure_(
      fn, r5js.procspec.EXACTLY_2_ARGS_, typeChecker);
};


/**
 * @param {function(T1, T2, T3): ?} fn
 * @param {!r5js.Type=} opt_argtype1
 * @param {!r5js.Type=} opt_argtype2
 * @param {!r5js.Type=} opt_argtype3
 * @return {!r5js.procspec.PrimitiveProcedure_}
 * @template T1,T2,T3
 * TODO bl make the template types mean something
 */
r5js.procspec.ternary = function(fn, opt_argtype1, opt_argtype2, opt_argtype3) {
  var argtypes = [];
  if (opt_argtype1) {
    argtypes.push(opt_argtype1);
  }
  if (opt_argtype2) {
    argtypes.push(opt_argtype2);
  }
  if (opt_argtype3) {
    argtypes.push(opt_argtype3);
  }
  var typeChecker = argtypes.length ?
      new r5js.procspec.ArgumentTypeCheckerAndUnwrapperImpl_(argtypes) :
      r5js.procspec.NO_TYPE_RESTRICTIONS_;
  return new r5js.procspec.PrimitiveProcedure_(
      fn, r5js.procspec.EXACTLY_3_ARGS_, typeChecker);
};


/**
 * @param {!Function} fn
 * @param {!r5js.Type} typeOfAllArgs
 * @return {!r5js.procspec.PrimitiveProcedure_}
 */
r5js.procspec.varargsAtLeast0 = function(fn, typeOfAllArgs) {
  return new r5js.procspec.PrimitiveProcedure_(
      fn, r5js.procspec.ANY_NUMBER_OF_ARGS_,
      new r5js.procspec.AllArgsOfType_(typeOfAllArgs));
};


/**
 * @param {!Function} fn
 * @param {!r5js.Type} typeOfAllArgs
 * @return {!r5js.procspec.PrimitiveProcedure_}
 */
r5js.procspec.varargsAtLeast1 = function(fn, typeOfAllArgs) {
  return new r5js.procspec.PrimitiveProcedure_(
      fn, r5js.procspec.AT_LEAST_1_ARG_,
      new r5js.procspec.AllArgsOfType_(typeOfAllArgs));
};


/**
 * @param {!Function} fn
 * @param {number} minArgs
 * @param {number} maxArgs
 * @return {!r5js.procspec.PrimitiveProcedure_}
 */
r5js.procspec.varargsRange = function(fn, minArgs, maxArgs) {
  return new r5js.procspec.PrimitiveProcedure_(
      fn, new r5js.procspec.Between_(minArgs, maxArgs),
      r5js.procspec.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(!r5js.InputPort, !r5js.OutputPort): ?} fn
 * @return {!r5js.procspec.PrimitiveProcedure_}
 */
r5js.procspec.nullaryWithCurrentPorts = function(fn) {
  return new r5js.procspec.NeedsCurrentPorts_(
      fn, new r5js.procspec.Exactly_(0),
      r5js.procspec.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(!r5js.InputPort, !r5js.OutputPort, ?, ?): ?} fn
 * @return {!r5js.procspec.PrimitiveProcedure_}
 */
r5js.procspec.binaryWithCurrentPorts = function(fn) {
  return new r5js.procspec.NeedsCurrentPorts_(
      fn, new r5js.procspec.Exactly_(2),
      r5js.procspec.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(!r5js.InputPort, !r5js.OutputPort, ?): ?} fn
 * @return {!r5js.procspec.PrimitiveProcedure_}
 */
r5js.procspec.nullaryOrUnaryWithCurrentPorts = function(fn) {
  return new r5js.procspec.NeedsCurrentPorts_(
      fn, new r5js.procspec.Between_(0, 1),
      r5js.procspec.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(!r5js.InputPort, !r5js.OutputPort, ?, ?): ?} fn
 * @return {!r5js.procspec.PrimitiveProcedure_}
 */
r5js.procspec.unaryOrBinaryWithCurrentPorts = function(fn) {
  return new r5js.procspec.NeedsCurrentPorts_(
      fn, new r5js.procspec.Between_(1, 2),
      r5js.procspec.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(?, !r5js.ProcCallLike, !r5js.TrampolineHelper): ?}  fn
 * @return {!r5js.procspec.PrimitiveProcedure_}
 */
r5js.procspec.unaryWithSpecialEvalLogic = function(fn) {
  return new r5js.procspec.HasSpecialEvalLogic_(
      fn, r5js.procspec.EXACTLY_1_ARG_, r5js.procspec.JUST_UNWRAP_ARGS_);
};


/**
 * @param {function(?, ?, !r5js.ProcCallLike, !r5js.TrampolineHelper): ?} fn
 * @return {!r5js.procspec.PrimitiveProcedure_}
 */
r5js.procspec.binaryWithSpecialEvalLogic = function(fn) {
  return new r5js.procspec.HasSpecialEvalLogic_(
      fn, r5js.procspec.EXACTLY_2_ARGS_, r5js.procspec.JUST_UNWRAP_ARGS_);
};


/**
 * @param {function(?, ?, ?, !r5js.ProcCallLike, !r5js.TrampolineHelper): ?} fn
 * @return {!r5js.procspec.PrimitiveProcedure_}
 */
r5js.procspec.ternaryWithSpecialEvalLogic = function(fn) {
  return new r5js.procspec.HasSpecialEvalLogic_(
      fn, r5js.procspec.EXACTLY_3_ARGS_, r5js.procspec.JUST_UNWRAP_ARGS_);
};


/**
 * @param {number} min
 * @param {!Function} fn
 * @return {!r5js.procspec.PrimitiveProcedure_}
 */
r5js.procspec.atLeastNWithSpecialEvalLogic = function(min, fn) {
  return new r5js.procspec.HasSpecialEvalLogic_(
      fn, new r5js.procspec.AtLeast_(min),
      r5js.procspec.NO_TYPE_RESTRICTIONS_);
};
