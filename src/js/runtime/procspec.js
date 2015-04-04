goog.provide('r5js.procspec');

goog.require('goog.array');
goog.require('r5js.Procedure');
goog.require('r5js.datumutil');
goog.require('r5js.error');

r5js.procspec.NumArgChecker_ = /** @private @interface */ class {
    /**
     * @param {number} numArgs
     * @param {string} nameToShowInErrorMessage
     */
    checkNumArgs(numArgs, nameToShowInErrorMessage) {}
};

r5js.procspec.Exactly_ = /** @private @implements {r5js.procspec.NumArgChecker_} */ class {
    /** @param {number} numArgs */
    constructor(numArgs) {
        /** @const @private {number} */
        this.numArgs_ = numArgs;
    }

    /** @override */
    checkNumArgs(numArgs, nameToShowInErrorMessage) {
        if (numArgs !== this.numArgs_) {
            throw r5js.error.incorrectNumArgs(
                nameToShowInErrorMessage, this.numArgs_, numArgs);
        }
    }
};

r5js.procspec.AtLeast_ = /** @private @implements {r5js.procspec.NumArgChecker_} */ class {
    /** @param {number} min */
    constructor(min) {
        this.min_ = min;
    }

    /** @override */
    checkNumArgs(numArgs, nameToShowInErrorMessage) {
        if (numArgs < this.min_) {
            throw r5js.error.tooFewVarargs(
                nameToShowInErrorMessage, this.min_, numArgs);
        }
    }
};

r5js.procspec.Between_ = /** @private @implements {r5js.procspec.NumArgChecker_} */ class {
    /**
     * @param {number} minArgs
     * @param {number} maxArgs
     */
    constructor(minArgs, maxArgs) {
        /** @const @private */ this.minArgs_ = minArgs;
        /** @const @private */ this.maxArgs_ = maxArgs;
    }

    /** @override */
    checkNumArgs(numArgs, nameToShowInErrorMessage) {
        if (numArgs < this.minArgs_) {
            throw r5js.error.tooFewVarargs(
                nameToShowInErrorMessage, this.minArgs_, numArgs);
        }
        if (numArgs > this.maxArgs_) {
            throw r5js.error.tooManyVarargs(
                nameToShowInErrorMessage, this.maxArgs_, numArgs);
        }
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



r5js.procspec.ArgumentTypeCheckerAndUnwrapper_ = /** @private @interface */ class {
    /**
     * @param {!goog.array.ArrayLike} args
     * @param {string} nameToShowInErrorMessage
     * @return {!goog.array.ArrayLike}
     */
    checkAndUnwrapArgs(args, nameToShowInErrorMessage) {}
};

r5js.procspec.ArgumentTypeCheckerAndUnwrapperImpl_ =
    /** @private @implements {r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} */ class {

        /** @param {!Array<!r5js.Type>} argtypes */
        constructor(argtypes) {
            /** @const @private {!Array<!r5js.Type>} */
            this.argtypes_ = argtypes;
        }

        /**
         * @override
         * @suppress {accessControls|checkTypes} for r5js.PrimitiveProcedures_
         */
        checkAndUnwrapArgs(args, nameToShowInErrorMessage) {
            const unwrappedArgs = [];
            for (let i = 0; i < this.argtypes_.length; ++i) {
                const arg = args[i];
                const expectedType = this.argtypes_[i];
                if (!r5js.PrimitiveProcedures.registry_[expectedType + '?'].fn_.call(
                        null, arg)) {
                    const actualType = r5js.PrimitiveProcedures.getActualType_(arg);
                    throw r5js.error.argumentTypeError(
                        arg, i, nameToShowInErrorMessage, expectedType, actualType);
                }
                unwrappedArgs.push(arg instanceof r5js.Datum ? arg.unwrap() : arg);
            }
            return unwrappedArgs;
        }
    };

r5js.procspec.NoTypeChecking_ =
    /** @private @implements {r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} */ class {
    /** @override */
    checkAndUnwrapArgs(args, nameToShowInErrorMessage) {
        return args;
    }
};

r5js.procspec.AllArgsOfType_ =
    /** @private @implements {r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} */ class {
        /** @param {!r5js.Type} type */
        constructor(type) {
            /** @const @private */ this.type_ = type;
        }

        /**
         * @override
         * @suppress {accessControls} TODO bl
         */
        checkAndUnwrapArgs(args, nameToShowInErrorMessage) {
            const argtype = this.type_;
            return goog.array.map(args, function (arg, i) {
                if (!(/** @type {!r5js.procspec.PrimitiveProcedure_} */ (
                        r5js.PrimitiveProcedures.registry_[argtype + '?'])).fn_.call(
                        null, arg)) {
                    throw r5js.error.argumentTypeError(
                        arg, i, nameToShowInErrorMessage, argtype,
                        r5js.PrimitiveProcedures.getActualType_(arg));
                }
                return arg instanceof r5js.Datum ? arg.unwrap() : arg;
            });
        }
    };

/** @const @private {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} */
r5js.procspec.NO_TYPE_RESTRICTIONS_ = new r5js.procspec.NoTypeChecking_();

/**
 * @implements {r5js.procspec.ArgumentTypeCheckerAndUnwrapper_}
 * @struct
 * @constructor
 * @private
 */
r5js.procspec.JustUnwrapArgs_ =
    /** @private @implements {r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} */ class {

        /** @override */
        checkAndUnwrapArgs(args, nameToShowInErrorMessage) {
            return args;
        }
    };

/** @const @private {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} */
r5js.procspec.JUST_UNWRAP_ARGS_ = new r5js.procspec.JustUnwrapArgs_();

r5js.procspec.PrimitiveProcedure_ = /** @private */ class extends r5js.Procedure {
    /**
     * @param {!Function} fn TODO bl narrow type?
     * @param {!r5js.procspec.NumArgChecker_} numArgChecker
     * @param {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} typeChecker
     */
    constructor(fn, numArgChecker, typeChecker) {
        /** @const @private {function(!r5js.Datum):?} */
        this.fn_ = fn;

        /** @const @private {!r5js.procspec.NumArgChecker_} */
        this.numArgChecker_ = numArgChecker;

        /** @const @private {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} */
        this.typeChecker_ = typeChecker;

        /** @private {string} */
        this.debugName_ = '';
    }

    /**
     * Procedures have no deep need to know their names, as they are only bindings
     * and can change: (set! car cdr). This method exists only to increase
     * the usefulness of error messages thrown from primitive procedures.
     * @param {string} name
     */
    setDebugName(name) {
        this.debugName_ = name;
    }

    /** @return {string} */
    getDebugName() {
        return this.debugName_;
    }

    /**
     * @param {!goog.array.ArrayLike} userArgs
     * @param {!r5js.ProcCallLike} procCallLike
     * @param {!r5js.TrampolineHelper} trampolineHelper
     * @protected
     */
    call(userArgs, procCallLike, trampolineHelper) {
        this.numArgChecker_.checkNumArgs(userArgs.length, this.debugName_);
        const unwrappedArgs = this.typeChecker_.checkAndUnwrapArgs(
            userArgs, this.debugName_);
        const ans = this.fn_.apply(null, unwrappedArgs);
        procCallLike.bindResult(ans);
        trampolineHelper.setValue(ans);
        const nextContinuable = procCallLike.getNext();
        if (nextContinuable) {
            trampolineHelper.setNext(nextContinuable);
        }
    }

    /**
     * Primitive procedure, represented by JavaScript function:
     * (+ x y [ans ...]). We perform the action ("+"), bind the
     * result to the continuation's result name ("ans"), and advance
     * to the next continuable ("...").
     * @override
     */
    evaluate(args, procCallLike, trampolineHelper, env) {
        args = args.map(/** @type {!Function} */ (
            r5js.datumutil.wrapValue));
        // todo bl document why we're doing this...
        for (let i = 0; i < args.length; ++i) {
            if (args[i] instanceof r5js.Ref) {
                args[i] = (/** @type {!r5js.Ref} */ (args[i])).deref();
            }
        }
        this.call(args, procCallLike, trampolineHelper);
    }
};

r5js.procspec.NeedsCurrentPorts_ = /** @private */ class extends r5js.procspec.PrimitiveProcedure_ {
    /**
     * @param {!Function} fn
     * @param {!r5js.procspec.NumArgChecker_} numArgChecker
     * @param {!r5js.procspec.ArgumentTypeCheckerAndUnwrapper_} typeChecker
     */
    constructor(fn, numArgChecker, typeChecker) {
        super(fn, numArgChecker, typeChecker);
    }

    /** @override */
    call(userArgs, procCallLike, trampolineHelper) {
        this.numArgChecker_.checkNumArgs(userArgs.length, this.debugName_);
        const unwrappedArgs = this.typeChecker_.checkAndUnwrapArgs(
            userArgs, this.debugName_);
        const args = goog.array.concat(
            trampolineHelper.getInputPort(),
            trampolineHelper.getOutputPort(),
            goog.array.toArray(unwrappedArgs));
        const ans = this.fn_.apply(null, args);
        procCallLike.bindResult(ans);
        trampolineHelper.setValue(ans);
        const nextContinuable = procCallLike.getNext();
        if (nextContinuable) {
            trampolineHelper.setNext(nextContinuable);
        }
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
  const unwrappedArgs = this.typeChecker_.checkAndUnwrapArgs(
      userArgs, this.debugName_);
  const args = goog.array.concat(
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
  const argtypes = [];
  if (goog.isDef(opt_argtype1)) {
    argtypes.push(opt_argtype1);
  }
  if (goog.isDef(opt_argtype2)) {
    argtypes.push(opt_argtype2);
  }
  const typeChecker = argtypes.length === 0
      ? r5js.procspec.NO_TYPE_RESTRICTIONS_
      : new r5js.procspec.ArgumentTypeCheckerAndUnwrapperImpl_(argtypes);
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
  const argtypes = [];
  if (opt_argtype1) {
    argtypes.push(opt_argtype1);
  }
  if (opt_argtype2) {
    argtypes.push(opt_argtype2);
  }
  if (opt_argtype3) {
    argtypes.push(opt_argtype3);
  }
  const typeChecker = argtypes.length
      ? new r5js.procspec.ArgumentTypeCheckerAndUnwrapperImpl_(argtypes)
      : r5js.procspec.NO_TYPE_RESTRICTIONS_;
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
