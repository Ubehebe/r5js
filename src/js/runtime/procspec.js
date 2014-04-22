goog.provide('r5js.procspec');


goog.require('goog.array');
goog.require('r5js.ArgumentTypeError');
goog.require('r5js.IncorrectNumArgs');
goog.require('r5js.ProcedureLike');
goog.require('r5js.TooFewArgs');
goog.require('r5js.TooManyArgs');
goog.require('r5js.datumutil');



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
    throw new r5js.IncorrectNumArgs(
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
    throw new r5js.TooFewArgs(
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
    throw new r5js.TooFewArgs(
        nameToShowInErrorMessage, this.minArgs_, numArgs);
  }
  if (numArgs > this.maxArgs_) {
    throw new r5js.TooManyArgs(
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
 * @param {!Array.<!r5js.Type>} argtypes
 * @implements {r5js.procspec.ArgumentTypeCheckerAndUnwrapper_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.ArgumentTypeCheckerAndUnwrapperImpl_ = function(argtypes) {
  /** @const @private {!Array.<!r5js.Type>} */
  this.argtypes_ = argtypes;
};


/**
 * @override
 * @suppress {accessControls} for r5js.PrimitiveProcedures_
 */
r5js.procspec.ArgumentTypeCheckerAndUnwrapperImpl_.prototype.
    checkAndUnwrapArgs = function(args, nameToShowInErrorMessage) {
  var unwrappedArgs = [];
  for (var i = 0; i < this.argtypes_.length; ++i) {
    var arg = args[i];
    var argtype = this.argtypes_[i];
    if (!r5js.PrimitiveProcedures.registry_[argtype + '?'].fn_.call(
        null, arg)) {
      throw new r5js.ArgumentTypeError(
          arg, i, nameToShowInErrorMessage, argtype);
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
    if (!r5js.PrimitiveProcedures.registry_[argtype + '?'].fn_.call(
        null, arg)) {
      throw new r5js.ArgumentTypeError(
          arg, i, nameToShowInErrorMessage, argtype);
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
 * @implements {r5js.ProcedureLike}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.PrimitiveProcedure_ = function(
    fn, numArgChecker, typeChecker) {
  /** @const @private {function(!r5js.Datum):?} */
  this.fn_ = fn;

  /** @const @private {!r5js.procspec.NumArgChecker_} */
  this.numArgChecker_ = numArgChecker;

  /** @const @private {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} */
  this.typeChecker_ = typeChecker;

  /** @private {string} */
  this.debugName_ = '';
};
r5js.ProcedureLike.addImplementation(r5js.procspec.PrimitiveProcedure_);


/**
 * Procedures have no deep need to know their names, as they are only bindings
 * and can change: (set! car cdr). This method exists only to increase
 * the usefulness of error messages thrown from primitive procedures.
 * @param {string} name
 */
r5js.procspec.PrimitiveProcedure_.prototype.setDebugName = function(name) {
  this.debugName_ = name;
};


/**
 * @param {!goog.array.ArrayLike} userArgs
 * @param {!r5js.ProcCall} procCall
 * @param {!r5js.Continuation} continuation
 * @param {!r5js.TrampolineHelper} trampolineHelper
 * @protected
 */
r5js.procspec.PrimitiveProcedure_.prototype.call = function(
    userArgs, procCall, continuation, trampolineHelper) {
  this.numArgChecker_.checkNumArgs(userArgs.length, this.debugName_);
  var unwrappedArgs = this.typeChecker_.checkAndUnwrapArgs(
      userArgs, this.debugName_);
  var retval = this.fn_.apply(null, unwrappedArgs);
  var ans = r5js.datumutil.maybeWrapResult(retval);
  procCall.bindResult(continuation, ans);
  trampolineHelper.setValue(ans);
  var nextContinuable = continuation.getNextContinuable();
  if (nextContinuable) {
    trampolineHelper.setNextProcCallLike(nextContinuable);
  }
};


/**
 * Primitive procedure, represented by JavaScript function:
 * (+ x y [ans ...]). We perform the action ("+"), bind the
 * result to the continuation's result name ("ans"), and advance
 * to the next continuable ("...").
 * @override
 */
r5js.procspec.PrimitiveProcedure_.prototype.evalAndAdvance =
    function(procCall, procCallLike, trampolineHelper, parserProvider) {

  var continuation = /** @type {!r5js.Continuation} */ (
      procCallLike.getContinuation());
  /* If the operands aren't simple, we'll have to take a detour to
             restructure them. Example:

             (+ (* 1 2) (/ 3 4)) => (* 1 2 [_0 (/ 3 4 [_1 (+ _0 _1 ...)])]) */
  if (!procCall.operandsInCpsStyle()) {
    procCall.cpsify(continuation, trampolineHelper, parserProvider);
  }

  else {
    var args = procCall.evalArgs(true);
    // todo bl document why we're doing this...
    for (var i = 0; i < args.length; ++i) {
      if (args[i] instanceof r5js.Ref) {
        args[i] = (/** @type {!r5js.Ref} */ (args[i])).deref();
      }
    }
    this.call(args, procCall, continuation, trampolineHelper);
  }
};



/**
 * @param {!Function} fn
 * @param {!r5js.procspec.NumArgChecker_} numArgChecker
 * @param {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} typeChecker
 * @implements {r5js.ProcedureLike}
 * @extends {r5js.procspec.PrimitiveProcedure_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.NeedsCurrentPorts_ = function(fn, numArgChecker, typeChecker) {
  goog.base(this, fn, numArgChecker, typeChecker);
};
goog.inherits(
    r5js.procspec.NeedsCurrentPorts_, r5js.procspec.PrimitiveProcedure_);
r5js.ProcedureLike.addImplementation(r5js.procspec.NeedsCurrentPorts_);


/** @override */
r5js.procspec.NeedsCurrentPorts_.prototype.call = function(
    userArgs, procCall, continuation, trampolineHelper) {
  this.numArgChecker_.checkNumArgs(userArgs.length, this.debugName_);
  var unwrappedArgs = this.typeChecker_.checkAndUnwrapArgs(
      userArgs, this.debugName_);
  var args = goog.array.concat(
      goog.array.toArray(unwrappedArgs),
      trampolineHelper.getInputPort(),
      trampolineHelper.getOutputPort());
  var retval = this.fn_.apply(null, args);
  var ans = r5js.datumutil.maybeWrapResult(retval);
  procCall.bindResult(continuation, ans);
  trampolineHelper.setValue(ans);
  var nextContinuable = continuation.getNextContinuable();
  if (nextContinuable) {
    trampolineHelper.setNextProcCallLike(nextContinuable);
  }
};



/**
 * @param {!Function} fn
 * @param {!r5js.procspec.NumArgChecker_} numArgChecker
 * @param {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} typeChecker
 * @implements {r5js.ProcedureLike}
 * @extends {r5js.procspec.PrimitiveProcedure_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.HasSpecialEvalLogic_ = function(fn, numArgChecker, typeChecker) {
  goog.base(this, fn, numArgChecker, typeChecker);
};
goog.inherits(
    r5js.procspec.HasSpecialEvalLogic_, r5js.procspec.PrimitiveProcedure_);
r5js.ProcedureLike.addImplementation(r5js.procspec.HasSpecialEvalLogic_);


/** @override */
r5js.procspec.HasSpecialEvalLogic_.prototype.call = function(
    userArgs, procCall, continuation, trampolineHelper) {
  this.numArgChecker_.checkNumArgs(userArgs.length, this.debugName_);
  var unwrappedArgs = this.typeChecker_.checkAndUnwrapArgs(
      userArgs, this.debugName_);
  var args = goog.array.concat(
      goog.array.toArray(unwrappedArgs),
      procCall, continuation, trampolineHelper);
  this.fn_.apply(null, args);
};


/**
 * @param {function(T):?} fn
 * @param {!r5js.Type=} opt_argtype
 * @return {!r5js.ProcedureLike}
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
 * @return {!r5js.ProcedureLike}
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
 * @param {function(?, ?, ?): ?} fn
 * @return {!r5js.ProcedureLike}
 */
r5js.procspec.ternary = function(fn) {
  return new r5js.procspec.PrimitiveProcedure_(
      fn, r5js.procspec.EXACTLY_3_ARGS_, r5js.procspec.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {!Function} fn
 * @param {!r5js.Type} typeOfAllArgs
 * @return {!r5js.ProcedureLike}
 */
r5js.procspec.varargsAtLeast0 = function(fn, typeOfAllArgs) {
  return new r5js.procspec.PrimitiveProcedure_(
      fn, r5js.procspec.ANY_NUMBER_OF_ARGS_,
      new r5js.procspec.AllArgsOfType_(typeOfAllArgs));
};


/**
 * @param {!Function} fn
 * @param {!r5js.Type} typeOfAllArgs
 * @return {!r5js.ProcedureLike}
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
 * @return {!r5js.ProcedureLike}
 */
r5js.procspec.varargsRange = function(fn, minArgs, maxArgs) {
  return new r5js.procspec.PrimitiveProcedure_(
      fn, new r5js.procspec.Between_(minArgs, maxArgs),
      r5js.procspec.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(!r5js.InputPort, !r5js.OutputPort): ?} fn
 * @return {!r5js.ProcedureLike}
 */
r5js.procspec.nullaryWithCurrentPorts = function(fn) {
  return new r5js.procspec.NeedsCurrentPorts_(
      fn, new r5js.procspec.Exactly_(0),
      r5js.procspec.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(?, !r5js.InputPort, !r5js.OutputPort): ?} fn
 * @return {!r5js.ProcedureLike}
 */
r5js.procspec.nullaryOrUnaryWithCurrentPorts = function(fn) {
  return new r5js.procspec.NeedsCurrentPorts_(
      fn, new r5js.procspec.Between_(0, 1),
      r5js.procspec.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(?, ?, !r5js.InputPort, !r5js.OutputPort): ?} fn
 * @return {!r5js.ProcedureLike}
 */
r5js.procspec.unaryOrBinaryWithCurrentPorts = function(fn) {
  return new r5js.procspec.NeedsCurrentPorts_(
      fn, new r5js.procspec.Between_(1, 2),
      r5js.procspec.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(?, !r5js.ProcCall, !r5js.Continuation, !r5js.TrampolineHelper): ?}  fn
 * @return {!r5js.ProcedureLike}
 */
r5js.procspec.unaryWithSpecialEvalLogic = function(fn) {
  return new r5js.procspec.HasSpecialEvalLogic_(
      fn, r5js.procspec.EXACTLY_1_ARG_, r5js.procspec.JUST_UNWRAP_ARGS_);
};


/**
 * @param {function(?, ?, !r5js.ProcCall, !r5js.Continuation, !r5js.TrampolineHelper): ?} fn
 * @return {!r5js.ProcedureLike}
 */
r5js.procspec.binaryWithSpecialEvalLogic = function(fn) {
  return new r5js.procspec.HasSpecialEvalLogic_(
      fn, r5js.procspec.EXACTLY_2_ARGS_, r5js.procspec.JUST_UNWRAP_ARGS_);
};


/**
 * @param {function(?, ?, ?, !r5js.ProcCall, !r5js.Continuation, !r5js.TrampolineHelper): ?} fn
 * @return {!r5js.ProcedureLike}
 */
r5js.procspec.ternaryWithSpecialEvalLogic = function(fn) {
  return new r5js.procspec.HasSpecialEvalLogic_(
      fn, r5js.procspec.EXACTLY_3_ARGS_, r5js.procspec.JUST_UNWRAP_ARGS_);
};


/**
 * @param {number} min
 * @param {!Function} fn
 * @return {!r5js.ProcedureLike}
 */
r5js.procspec.atLeastNWithSpecialEvalLogic = function(min, fn) {
  return new r5js.procspec.HasSpecialEvalLogic_(
      fn, new r5js.procspec.AtLeast_(min),
      r5js.procspec.NO_TYPE_RESTRICTIONS_);
};
