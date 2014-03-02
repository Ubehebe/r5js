goog.provide('r5js.runtime.unary');


goog.require('goog.array');
goog.require('r5js.ArgumentTypeError');
goog.require('r5js.IncorrectNumArgs');
goog.require('r5js.data');



/** @interface */
r5js.runtime.PrimitiveProcedure = function() {};


/**
 * @type {!Function}
 * TODO bl improve name.
 */
r5js.runtime.PrimitiveProcedure.prototype.javascript;



/**
 * @interface
 * @private
 */
r5js.runtime.NumArgChecker_ = function() {};


/** @param {number} numArgs */
r5js.runtime.NumArgChecker_.prototype.checkNumArgs = function(numArgs) {};



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
r5js.runtime.Exactly_.prototype.checkNumArgs = function(numArgs) {
  if (numArgs !== this.numArgs_) {
    throw new r5js.IncorrectNumArgs(
        'blah' /* TODO bl */, this.numArgs_, numArgs);
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
r5js.runtime.AtLeast_.prototype.checkNumArgs = function(numArgs) {
  if (numArgs < this.min_) {
    throw new r5js.TooFewArgs('blah' /* TODO bl */, this.min_, numArgs);
  }
};


/** @const @private {!r5js.runtime.NumArgChecker_} */
r5js.runtime.EXACTLY_1_ARG_ = new r5js.runtime.Exactly_(1);


/** @const @private {!r5js.runtime.NumArgChecker_} */
r5js.runtime.EXACTLY_2_ARGS_ = new r5js.runtime.Exactly_(2);


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
 * @return {!goog.array.ArrayLike}
 */
r5js.runtime.ArgumentTypeCheckerAndUnwrapper_.prototype.checkAndUnwrapArgs =
    function(args) {};



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
    function(args) {
  var unwrappedArgs = [];
  for (var i = 0; i < this.argtypes_.length; ++i) {
    var arg = args[i];
    var argtype = this.argtypes_[i];
    if (!r5js.runtime.PrimitiveProcedures_[argtype + '?'].fn_.call(null, arg)) {
      throw new r5js.ArgumentTypeError(
          arg, i, 'blah' /* TODO bl */, argtype);
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
r5js.runtime.NoTypeChecking_.prototype.checkAndUnwrapArgs = function(args) {
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
r5js.runtime.AllArgsOfType_.prototype.checkAndUnwrapArgs = function(args) {
  var argtype = this.type_;
  return goog.array.map(args, function(arg, i) {
    if (!r5js.runtime.PrimitiveProcedures_[argtype + '?'].fn_.call(null, arg)) {
      throw new r5js.ArgumentTypeError(
          arg, i, 'blah' /* TODO bl */, argtype);
    }
    return arg instanceof r5js.Datum ? arg.unwrap() : arg;
  });
};



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
};


/** @override */
r5js.runtime.PrimitiveProcedure.Base_.prototype.javascript = function() {
  this.numArgChecker_.checkNumArgs(arguments.length);
  var args = this.typeChecker_.checkAndUnwrapArgs(arguments);
  var retval = this.fn_.apply(null, args);
  return r5js.data.maybeWrapResult(retval);
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
      fn, r5js.runtime.EXACTLY_2_ARGS_, typeChecker);
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
