goog.provide('r5js.runtime.binary');
goog.provide('r5js.runtime.unary');
goog.provide('r5js.runtime.varargsAtLeast0');
goog.provide('r5js.runtime.varargsAtLeast1');


goog.require('goog.array');
goog.require('r5js.ArgumentTypeError');
goog.require('r5js.IncorrectNumArgs');
goog.require('r5js.TooFewArgs');
goog.require('r5js.TooManyArgs');
goog.require('r5js.data');



/** @interface */
r5js.runtime.PrimitiveProcedure = function() {};


/**
 * @return {!Function}
 * TODO bl improve name.
 */
r5js.runtime.PrimitiveProcedure.prototype.getBoundJavascript = function() {};


/**
 * Procedures have no deep need to know their names, as they are only bindings
 * and can change: (set! car cdr). This method exists only to increase
 * the usefulness of error messages thrown from primitive procedures.
 * @param {string} name
 */
r5js.runtime.PrimitiveProcedure.prototype.setDebugName = function(name) {};



/**
 * @interface
 * @private
 */
r5js.runtime.NumArgChecker_ = function() {};


/**
 * @param {number} numArgs
 * @param {string} nameToShowInErrorMessage
 */
r5js.runtime.NumArgChecker_.prototype.checkNumArgs = function(
    numArgs, nameToShowInErrorMessage) {};



/**
 * @param {number} numArgs
 * @implements {r5js.runtime.NumArgChecker_}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.Exactly_ = function(numArgs) {
  /** @const @private {number} */
  this.numArgs_ = numArgs;
};


/** @override */
r5js.runtime.Exactly_.prototype.checkNumArgs = function(
    numArgs, nameToShowInErrorMessage) {
  if (numArgs !== this.numArgs_) {
    throw new r5js.IncorrectNumArgs(
        nameToShowInErrorMessage, this.numArgs_, numArgs);
  }
};



/**
 * @param {number} numUserArgs
 * @param {number} numArgsAddedByRuntime
 * @implements {r5js.runtime.NumArgChecker_}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.ExactlyWithExtraRuntimeArgs_ = function(
    numUserArgs, numArgsAddedByRuntime) {
  /** @const @private {number} */
  this.numUserArgs_ = numUserArgs;

  /** @const @private {number} */
  this.numArgsAddedByRuntime_ = numArgsAddedByRuntime;
};


/** @override */
r5js.runtime.ExactlyWithExtraRuntimeArgs_.prototype.checkNumArgs = function(
    numArgs, nameToShowInErrorMessage) {
  var numArgsToExpect = this.numUserArgs_ + this.numArgsAddedByRuntime_;
  if (numArgsToExpect !== numArgs) {
    throw new r5js.IncorrectNumArgs(
        nameToShowInErrorMessage,
        this.numUserArgs_,
        numArgs - this.numArgsAddedByRuntime_);
  }
};



/**
 * @param {number} min
 * @implements {r5js.runtime.NumArgChecker_}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.AtLeast_ = function(min) {
  this.min_ = min;
};


/** @override */
r5js.runtime.AtLeast_.prototype.checkNumArgs = function(
    numArgs, nameToShowInErrorMessage) {
  if (numArgs < this.min_) {
    throw new r5js.TooFewArgs(
        nameToShowInErrorMessage, this.min_, numArgs);
  }
};



/**
 * @param {number} minArgs
 * @param {number} maxArgs
 * @implements {r5js.runtime.NumArgChecker_}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.Between_ = function(minArgs, maxArgs) {
  /** @const @private {number} */
  this.minArgs_ = minArgs;

  /** @const @private {number} */
  this.maxArgs_ = maxArgs;
};


/** @override */
r5js.runtime.Between_.prototype.checkNumArgs = function(
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



/**
 * @param {number} minNumUserArgs
 * @param {number} maxNumUserArgs
 * @param {number} numArgsAddedByRuntime
 * @implements {r5js.runtime.NumArgChecker_}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.BetweenWithExtraRuntimeArgs_ = function(
    minNumUserArgs, maxNumUserArgs, numArgsAddedByRuntime) {
  /** @const @private {number} */
  this.minNumUserArgs_ = minNumUserArgs;

  /** @const @private {number} */
  this.maxNumUserArgs_ = maxNumUserArgs;

  /** @const @private {number} */
  this.numArgsAddedByRuntime_ = numArgsAddedByRuntime;
};


/** @override */
r5js.runtime.BetweenWithExtraRuntimeArgs_.prototype.checkNumArgs = function(
    numArgs, nameToShowInErrorMessage) {
  var minNumArgs = this.minNumUserArgs_ + this.numArgsAddedByRuntime_;
  var maxNumArgs = this.maxNumUserArgs_ + this.numArgsAddedByRuntime_;
  if (numArgs < minNumArgs) {
    throw new r5js.TooFewArgs(
        nameToShowInErrorMessage,
        this.minNumUserArgs_,
        numArgs - this.numArgsAddedByRuntime_);
  }
  if (numArgs > maxNumArgs) {
    throw new r5js.TooManyArgs(
        nameToShowInErrorMessage,
        this.maxNumUserArgs_,
        numArgs - this.numArgsAddedByRuntime_);
  }
};



/**
 * @param {number} minNumUserArgs
 * @param {number} numArgsAddedByRuntime
 * @implements {r5js.runtime.NumArgChecker_}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.AtLeastWithExtraRuntimeArgs_ = function(
    minNumUserArgs, numArgsAddedByRuntime) {
  /** @const @private {number} */
  this.numArgsAddedByRuntime_ = numArgsAddedByRuntime;

  /** @const @private {number} */
  this.minNumUserArgs_ = minNumUserArgs;
};


/** @override */
r5js.runtime.AtLeastWithExtraRuntimeArgs_.prototype.checkNumArgs = function(
    numArgs, nameToShowInErrorMessage) {
  var minNumArgs = this.minNumUserArgs_ + this.numArgsAddedByRuntime_;
  if (numArgs < minNumArgs) {
    throw new r5js.TooFewArgs(
        nameToShowInErrorMessage,
        this.minNumUserArgs_,
        numArgs - this.numArgsAddedByRuntime_);
  }
};


/** @const @private {!r5js.runtime.NumArgChecker_} */
r5js.runtime.EXACTLY_1_ARG_ = new r5js.runtime.Exactly_(1);


/** @const @private {!r5js.runtime.NumArgChecker_} */
r5js.runtime.EXACTLY_2_ARGS_ = new r5js.runtime.Exactly_(2);


/** @const @private {!r5js.runtime.NumArgChecker_} */
r5js.runtime.EXACTLY_3_ARGS_ = new r5js.runtime.Exactly_(3);


/** @const @private {!r5js.runtime.NumArgChecker_} */
r5js.runtime.ANY_NUMBER_OF_ARGS_ = new r5js.runtime.AtLeast_(0);


/** @const @private {!r5js.runtime.NumArgChecker_} */
r5js.runtime.AT_LEAST_1_ARG_ = new r5js.runtime.AtLeast_(1);



/**
 * @interface
 * @private
 */
r5js.runtime.ArgumentTypeCheckerAndUnwrapper_ = function() {};


/**
 * @param {!goog.array.ArrayLike} args
 * @param {string} nameToShowInErrorMessage
 * @return {!goog.array.ArrayLike}
 */
r5js.runtime.ArgumentTypeCheckerAndUnwrapper_.prototype.checkAndUnwrapArgs =
    function(args, nameToShowInErrorMessage) {};



/**
 * @param {!Array.<!r5js.Type>} argtypes
 * @implements {r5js.runtime.ArgumentTypeCheckerAndUnwrapper_}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.ArgumentTypeCheckerAndUnwrapperImpl_ = function(argtypes) {
  /** @const @private {!Array.<!r5js.Type>} */
  this.argtypes_ = argtypes;
};


/**
 * @override
 * @suppress {accessControls} for r5js.runtime.PrimitiveProcedures_
 */
r5js.runtime.ArgumentTypeCheckerAndUnwrapperImpl_.prototype.checkAndUnwrapArgs =
    function(args, nameToShowInErrorMessage) {
  var unwrappedArgs = [];
  for (var i = 0; i < this.argtypes_.length; ++i) {
    var arg = args[i];
    var argtype = this.argtypes_[i];
    if (!r5js.runtime.PrimitiveProcedures_[argtype + '?'].fn_.call(null, arg)) {
      throw new r5js.ArgumentTypeError(
          arg, i, nameToShowInErrorMessage, argtype);
    }
    unwrappedArgs.push(arg instanceof r5js.Datum ? arg.unwrap() : arg);
  }
  return unwrappedArgs;
};



/**
 * @implements {r5js.runtime.ArgumentTypeCheckerAndUnwrapper_}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.NoTypeChecking_ = function() {};


/** @override */
r5js.runtime.NoTypeChecking_.prototype.checkAndUnwrapArgs = function(
    args, nameToShowInErrorMessage) {
  return args;
};



/**
 * @param {!r5js.Type} type
 * @implements {r5js.runtime.ArgumentTypeCheckerAndUnwrapper_}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.AllArgsOfType_ = function(type) {
  /** @const @private {!r5js.Type} */
  this.type_ = type;
};


/** @const @private {!r5js.runtime.ArgumentTypeCheckerAndUnwrapper_} */
r5js.runtime.NO_TYPE_RESTRICTIONS_ = new r5js.runtime.NoTypeChecking_();


/**
 * @override
 * @suppress {accessControls} TODO bl
 */
r5js.runtime.AllArgsOfType_.prototype.checkAndUnwrapArgs = function(
    args, nameToShowInErrorMessage) {
  var argtype = this.type_;
  return goog.array.map(args, function(arg, i) {
    if (!r5js.runtime.PrimitiveProcedures_[argtype + '?'].fn_.call(null, arg)) {
      throw new r5js.ArgumentTypeError(
          arg, i, nameToShowInErrorMessage, argtype);
    }
    return arg instanceof r5js.Datum ? arg.unwrap() : arg;
  });
};



/**
 * @implements {r5js.runtime.ArgumentTypeCheckerAndUnwrapper_}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.JustUnwrapArgs_ = function() {};


/** @override */
r5js.runtime.JustUnwrapArgs_.prototype.checkAndUnwrapArgs = function(
    args, nameToShowInErrorMessage) {
  return goog.array.map(args, function(arg) {
    return arg instanceof r5js.Datum ?
        arg.unwrap() : arg;
  });
};


/** @const @private {!r5js.runtime.ArgumentTypeCheckerAndUnwrapper_} */
r5js.runtime.JUST_UNWRAP_ARGS_ = new r5js.runtime.JustUnwrapArgs_();



/**
 * @param {!Function} fn TODO bl narrow type?
 * @param {!r5js.runtime.NumArgChecker_} numArgChecker
 * @param {!r5js.runtime.ArgumentTypeCheckerAndUnwrapper_} typeChecker
 * @implements {r5js.runtime.PrimitiveProcedure}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.PrimitiveProcedure.Base_ = function(
    fn, numArgChecker, typeChecker) {
  /** @const @private {function(!r5js.Datum):?} */
  this.fn_ = fn;

  /** @const @private {!r5js.runtime.NumArgChecker_} */
  this.numArgChecker_ = numArgChecker;

  /** @const @private {!r5js.runtime.ArgumentTypeCheckerAndUnwrapper_} */
  this.typeChecker_ = typeChecker;

  /** @private {string} */
  this.debugName_ = '';
};


/** @override */
r5js.runtime.PrimitiveProcedure.Base_.prototype.setDebugName = function(name) {
  this.debugName_ = name;
};


/** @override */
r5js.runtime.PrimitiveProcedure.Base_.prototype.getBoundJavascript =
    function() {
  return goog.bind(this.javascript_, this);
};


/**
 * @return {r5js.PayloadType}
 * @private
 */
r5js.runtime.PrimitiveProcedure.Base_.prototype.javascript_ = function() {
  this.numArgChecker_.checkNumArgs(arguments.length, this.debugName_);
  var unwrappedArgs = this.typeChecker_.checkAndUnwrapArgs(
      arguments, this.debugName_);
  var retval = this.fn_.apply(null, unwrappedArgs);
  return r5js.data.maybeWrapResult(retval);
};



/**
 * @param {!Function} fn
 * @param {!r5js.runtime.NumArgChecker_} numArgChecker
 * @param {!r5js.runtime.ArgumentTypeCheckerAndUnwrapper_} typeChecker
 * @extends {r5js.runtime.PrimitiveProcedure.Base_}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.NeedsCurrentPorts_ = function(fn, numArgChecker, typeChecker) {
  goog.base(this, fn, numArgChecker, typeChecker);
};
goog.inherits(
    r5js.runtime.NeedsCurrentPorts_, r5js.runtime.PrimitiveProcedure.Base_);


/** @override */
r5js.runtime.NeedsCurrentPorts_.prototype.getBoundJavascript = function() {
  // TODO bl remove once legacy primitive procedures are all migrated.
  // Will also need to update r5js.ProcCall#tryPrimitiveProcedure.
  var bound = goog.base(this, 'getBoundJavascript');
  bound.needsCurrentPorts = true;
  return bound;
};



/**
 * @param {!Function} fn
 * @param {!r5js.runtime.NumArgChecker_} numArgChecker
 * @param {!r5js.runtime.ArgumentTypeCheckerAndUnwrapper_} typeChecker
 * @extends {r5js.runtime.PrimitiveProcedure.Base_}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.HasSpecialEvalLogic_ = function(fn, numArgChecker, typeChecker) {
  goog.base(this, fn, numArgChecker, typeChecker);
};
goog.inherits(
    r5js.runtime.HasSpecialEvalLogic_, r5js.runtime.PrimitiveProcedure.Base_);


/** @override */
r5js.runtime.HasSpecialEvalLogic_.prototype.getBoundJavascript = function() {
  var bound = goog.base(this, 'getBoundJavascript');
  bound.hasSpecialEvalLogic = true;
  return bound;
};


/**
 * @param {function(T):?} fn
 * @param {!r5js.Type=} opt_argtype
 * @return {!r5js.runtime.PrimitiveProcedure}
 * @template T
 * TODO bl: make the template type mean something
 */
r5js.runtime.unary = function(fn, opt_argtype) {
  return new r5js.runtime.PrimitiveProcedure.Base_(
      fn, r5js.runtime.EXACTLY_1_ARG_,
      goog.isDef(opt_argtype) ?
      new r5js.runtime.ArgumentTypeCheckerAndUnwrapperImpl_([opt_argtype]) :
      r5js.runtime.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(T1, T2):?} fn
 * @param {!r5js.Type=} opt_argtype1
 * @param {!r5js.Type=} opt_argtype2
 * @return {!r5js.runtime.PrimitiveProcedure}
 * @template T1,T2
 * TODO bl: make the template types mean something
 */
r5js.runtime.binary = function(fn, opt_argtype1, opt_argtype2) {
  var argtypes = [];
  if (goog.isDef(opt_argtype1)) {
    argtypes.push(opt_argtype1);
  }
  if (goog.isDef(opt_argtype2)) {
    argtypes.push(opt_argtype2);
  }
  var typeChecker = argtypes.length === 0 ?
      r5js.runtime.NO_TYPE_RESTRICTIONS_ :
      new r5js.runtime.ArgumentTypeCheckerAndUnwrapperImpl_(argtypes);
  return new r5js.runtime.PrimitiveProcedure.Base_(
      fn,
      r5js.runtime.EXACTLY_2_ARGS_,
      typeChecker);
};


/**
 * @param {function(?, ?, ?): ?} fn
 * @return {!r5js.runtime.PrimitiveProcedure}
 */
r5js.runtime.ternary = function(fn) {
  return new r5js.runtime.PrimitiveProcedure.Base_(
      fn,
      r5js.runtime.EXACTLY_3_ARGS_,
      r5js.runtime.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {!Function} fn
 * @param {!r5js.Type} typeOfAllArgs
 * @return {!r5js.runtime.PrimitiveProcedure}
 */
r5js.runtime.varargsAtLeast0 = function(fn, typeOfAllArgs) {
  return new r5js.runtime.PrimitiveProcedure.Base_(
      fn, r5js.runtime.ANY_NUMBER_OF_ARGS_,
      new r5js.runtime.AllArgsOfType_(typeOfAllArgs));
};


/**
 * @param {!Function} fn
 * @param {!r5js.Type} typeOfAllArgs
 * @return {!r5js.runtime.PrimitiveProcedure}
 */
r5js.runtime.varargsAtLeast1 = function(fn, typeOfAllArgs) {
  return new r5js.runtime.PrimitiveProcedure.Base_(
      fn, r5js.runtime.AT_LEAST_1_ARG_,
      new r5js.runtime.AllArgsOfType_(typeOfAllArgs));
};


/**
 * @param {!Function} fn
 * @param {number} minArgs
 * @param {number} maxArgs
 * @return {!r5js.runtime.PrimitiveProcedure}
 */
r5js.runtime.varargsRange = function(fn, minArgs, maxArgs) {
  return new r5js.runtime.PrimitiveProcedure.Base_(
      fn,
      new r5js.runtime.Between_(minArgs, maxArgs),
      r5js.runtime.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(!r5js.ast.InputPort, !r5js.ast.OutputPort): ?} fn
 * @return {!r5js.runtime.PrimitiveProcedure}
 */
r5js.runtime.nullaryWithCurrentPorts = function(fn) {
  return new r5js.runtime.NeedsCurrentPorts_(
      fn, new r5js.runtime.ExactlyWithExtraRuntimeArgs_(0, 2),
      r5js.runtime.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(?, !r5js.ast.InputPort, !r5js.ast.OutputPort): ?} fn
 * @return {!r5js.runtime.PrimitiveProcedure}
 */
r5js.runtime.nullaryOrUnaryWithCurrentPorts = function(fn) {
  return new r5js.runtime.NeedsCurrentPorts_(
      fn, new r5js.runtime.BetweenWithExtraRuntimeArgs_(0, 1, 2),
      r5js.runtime.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(?, ?, !r5js.ast.InputPort, !r5js.ast.OutputPort): ?} fn
 * @return {!r5js.runtime.PrimitiveProcedure}
 */
r5js.runtime.unaryOrBinaryWithCurrentPorts = function(fn) {
  return new r5js.runtime.NeedsCurrentPorts_(
      fn, new r5js.runtime.BetweenWithExtraRuntimeArgs_(1, 2, 2),
      r5js.runtime.NO_TYPE_RESTRICTIONS_);
};


/**
 * @param {function(?, !r5js.ProcCall, !r5js.Continuation, !r5js.TrampolineHelper): ?}  fn
 * @return {!r5js.runtime.PrimitiveProcedure}
 */
r5js.runtime.unaryWithSpecialEvalLogic = function(fn) {
  return new r5js.runtime.HasSpecialEvalLogic_(
      fn, new r5js.runtime.ExactlyWithExtraRuntimeArgs_(1, 3),
      r5js.runtime.JUST_UNWRAP_ARGS_);
};


/**
 * @param {function(?, ?, !r5js.ProcCall, !r5js.Continuation, !r5js.TrampolineHelper): ?} fn
 * @return {!r5js.runtime.PrimitiveProcedure}
 */
r5js.runtime.binaryWithSpecialEvalLogic = function(fn) {
  return new r5js.runtime.HasSpecialEvalLogic_(
      fn, new r5js.runtime.ExactlyWithExtraRuntimeArgs_(2, 3),
      r5js.runtime.JUST_UNWRAP_ARGS_);
};


/**
 * @param {function(?, ?, ?, !r5js.ProcCall, !r5js.Continuation, !r5js.TrampolineHelper): ?} fn
 * @return {!r5js.runtime.PrimitiveProcedure}
 */
r5js.runtime.ternaryWithSpecialEvalLogic = function(fn) {
  return new r5js.runtime.HasSpecialEvalLogic_(
      fn, new r5js.runtime.ExactlyWithExtraRuntimeArgs_(3, 3),
      r5js.runtime.JUST_UNWRAP_ARGS_);
};


/**
 * @param {number} min
 * @param {!Function} fn
 * @return {!r5js.runtime.PrimitiveProcedure}
 */
r5js.runtime.atLeastNWithSpecialEvalLogic = function(min, fn) {
  return new r5js.runtime.HasSpecialEvalLogic_(
      fn, new r5js.runtime.AtLeastWithExtraRuntimeArgs_(min, 3),
      r5js.runtime.NO_TYPE_RESTRICTIONS_);
};



