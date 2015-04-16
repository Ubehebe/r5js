goog.module('r5js.procspec');

const array = goog.require('goog.array');
const Boolean = goog.require('r5js.ast.Boolean');
const Character = goog.require('r5js.ast.Character');
const Continuation = goog.require('r5js.Continuation');
const Datum = goog.require('r5js.Datum');
const datumutil = goog.require('r5js.datumutil');
const Error = goog.require('r5js.Error');
const Identifier = goog.require('r5js.ast.Identifier');
const InputPort = goog.require('r5js.InputPort');
const IPair = goog.require('r5js.IPair');
const Lambda = goog.require('r5js.ast.Lambda');
const List = goog.require('r5js.ast.List');
const Number = goog.require('r5js.ast.Number');
const OutputPort = goog.require('r5js.OutputPort');
const ProcCallLike = goog.require('r5js.ProcCallLike');
const Procedure = goog.require('r5js.Procedure');
const Ref = goog.require('r5js.Ref');
const String = goog.require('r5js.ast.String');
const TrampolineHelper = goog.require('r5js.TrampolineHelper');
const Type = goog.require('r5js.Type');
const Vector = goog.require('r5js.ast.Vector');


/** @interface */
class NumArgChecker {
    /**
     * @param {number} numArgs
     * @param {string} nameToShowInErrorMessage
     */
    checkNumArgs(numArgs, nameToShowInErrorMessage) {}
}

/** @implements {NumArgChecker} */
class Exactly {
    /** @param {number} numArgs */
    constructor(numArgs) {
        /** @const @private */ this.numArgs_ = numArgs;
    }

    /** @override */
    checkNumArgs(numArgs, nameToShowInErrorMessage) {
        if (numArgs !== this.numArgs_) {
            throw Error.incorrectNumArgs(nameToShowInErrorMessage, this.numArgs_, numArgs);
        }
    }
}

/** @implements {NumArgChecker} */
class AtLeast {
    /** @param {number} min */
    constructor(min) {
        /** @const @private */ this.min_ = min;
    }

    /** @override */
    checkNumArgs(numArgs, nameToShowInErrorMessage) {
        if (numArgs < this.min_) {
            throw Error.tooFewVarargs(nameToShowInErrorMessage, this.min_, numArgs);
        }
    }
}

/** @implements {NumArgChecker} */
class Between {
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
            throw Error.tooFewVarargs(nameToShowInErrorMessage, this.minArgs_, numArgs);
        }
        if (numArgs > this.maxArgs_) {
            throw Error.tooManyVarargs(nameToShowInErrorMessage, this.maxArgs_, numArgs);
        }
    }
}

/** @const {!NumArgChecker} */ const EXACTLY_1_ARG = new Exactly(1);
/** @const {!NumArgChecker} */ const EXACTLY_2_ARGS = new Exactly(2);
/** @const {!NumArgChecker} */ const EXACTLY_3_ARGS = new Exactly(3);
/** @const {!NumArgChecker} */ const ANY_NUMBER_OF_ARGS = new AtLeast(0);
/** @const {!NumArgChecker} */ const AT_LEAST_1_ARG = new AtLeast(1);

/** @interface */
class ArgumentTypeCheckerAndUnwrapper {
    /**
     * @param {!array.ArrayLike} args
     * @param {string} nameToShowInErrorMessage
     * @return {!array.ArrayLike}
     */
    checkAndUnwrapArgs(args, nameToShowInErrorMessage) {}
}

/** @implements {ArgumentTypeCheckerAndUnwrapper} */
class ArgumentTypeCheckerAndUnwrapperImpl {

    /** @param {!Array<!Type>} argtypes */
    constructor(argtypes) {
        /** @const @private {!Array<!Type>} */
        this.argtypes_ = argtypes;
    }

    /**
     * @override
     * @suppress {accessControls} for r5js.PrimitiveProcedures_
     */
    checkAndUnwrapArgs(args, nameToShowInErrorMessage) {
        const unwrappedArgs = [];
        for (let i = 0; i < this.argtypes_.length; ++i) {
            const arg = args[i];
            const expectedType = this.argtypes_[i];
            if (!Predicates[expectedType.getName() + '?'].fn_.call(null, arg)) {
                const actualType = runtimeType(arg);
                throw Error.argumentTypeError(arg, i, nameToShowInErrorMessage, expectedType, actualType);
            }
            unwrappedArgs.push(arg instanceof Datum ? arg.unwrap() : arg);
        }
        return unwrappedArgs;
    }
}

/** @implements {ArgumentTypeCheckerAndUnwrapper} */
class NoTypeChecking {
    /** @override */
    checkAndUnwrapArgs(args, nameToShowInErrorMessage) {
        return args;
    }
}

/** @const {!ArgumentTypeCheckerAndUnwrapper} */ const NO_TYPE_RESTRICTIONS = new NoTypeChecking();

/** @implements {ArgumentTypeCheckerAndUnwrapper} */
class AllArgsOfType {
    /** @param {!Type} type */
    constructor(type) {
        /** @const @private */ this.type_ = type;
    }

    /** @override */
    checkAndUnwrapArgs(args, nameToShowInErrorMessage) {
        const argtype = this.type_;
        return goog.array.map(args, function (arg, i) {
            if (!(/** @type {!PrimitiveProcedure} */ (Predicates[argtype.getName() + '?'])).fn_.call(null, arg)) {
                throw Error.argumentTypeError(
                    arg, i, nameToShowInErrorMessage, argtype, runtimeType(arg));
            }
            return arg instanceof Datum ? arg.unwrap() : arg;
        });
    }
}


/**
 * @implements {ArgumentTypeCheckerAndUnwrapper}
 * TODO bl shouldn't this do type checking?
 */
class JustUnwrapArgs {
    /** @override */
    checkAndUnwrapArgs(args, nameToShowInErrorMessage) {
        return args;
    }
}

/** @const {!ArgumentTypeCheckerAndUnwrapper} */ const JUST_UNWRAP_ARGS = new JustUnwrapArgs();

class PrimitiveProcedure extends Procedure {
    /**
     * @param {!Function} fn TODO bl narrow type?
     * @param {!NumArgChecker} numArgChecker
     * @param {!ArgumentTypeCheckerAndUnwrapper} typeChecker
     */
    constructor(fn, numArgChecker, typeChecker) {
        /** @const @private {function(!Datum):?} */ this.fn_ = fn;
        /** @const @private {!NumArgChecker} */ this.numArgChecker_ = numArgChecker;
        /** @const @private {!ArgumentTypeCheckerAndUnwrapper} */ this.typeChecker_ = typeChecker;
        /** @private {string} */ this.debugName_ = '';
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
     * @param {!array.ArrayLike} userArgs
     * @param {!ProcCallLike} procCallLike
     * @param {!TrampolineHelper} trampolineHelper
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
        args = args.map(/** @type {!Function} */ (datumutil.wrapValue));
        // todo bl document why we're doing this...
        for (let i = 0; i < args.length; ++i) {
            if (args[i] instanceof Ref) {
                args[i] = (/** @type {!Ref} */ (args[i])).deref();
            }
        }
        this.call(args, procCallLike, trampolineHelper);
    }
}

class NeedsCurrentPorts extends PrimitiveProcedure {
    /**
     * @param {!Function} fn
     * @param {!NumArgChecker} numArgChecker
     * @param {!ArgumentTypeCheckerAndUnwrapper} typeChecker
     */
    constructor(fn, numArgChecker, typeChecker) {
        super(fn, numArgChecker, typeChecker);
    }

    /** @override */
    call(userArgs, procCallLike, trampolineHelper) {
        this.numArgChecker_.checkNumArgs(userArgs.length, this.debugName_);
        const unwrappedArgs = this.typeChecker_.checkAndUnwrapArgs(
            userArgs, this.debugName_);
        const args = array.concat(
            trampolineHelper.getInputPort(),
            trampolineHelper.getOutputPort(),
            array.toArray(unwrappedArgs));
        const ans = this.fn_.apply(null, args);
        procCallLike.bindResult(ans);
        trampolineHelper.setValue(ans);
        const nextContinuable = procCallLike.getNext();
        if (nextContinuable) {
            trampolineHelper.setNext(nextContinuable);
        }
    }
}

class HasSpecialEvalLogic extends PrimitiveProcedure {
    /**
     * @param {!Function} fn
     * @param {!NumArgChecker} numArgChecker
     * @param {!ArgumentTypeCheckerAndUnwrapper} typeChecker
     */
    constructor(fn, numArgChecker, typeChecker) {
        super(fn, numArgChecker, typeChecker);
    }

    /** @override */
    call(userArgs, procCallLike, trampolineHelper) {
        this.numArgChecker_.checkNumArgs(userArgs.length, this.debugName_);
        const unwrappedArgs = this.typeChecker_.checkAndUnwrapArgs(
            userArgs, this.debugName_);
        const args = array.concat(array.toArray(unwrappedArgs), procCallLike, trampolineHelper);
        this.fn_.apply(null, args);
    }
}

/**
 * @param {function(T):?} fn
 * @param {!Type=} opt_argtype
 * @return {!PrimitiveProcedure}
 * @template T
 * TODO bl: make the template type mean something
 */
function unary(fn, opt_argtype) {
    return new PrimitiveProcedure(
        fn,
        EXACTLY_1_ARG,
        goog.isDef(opt_argtype)
            ? new ArgumentTypeCheckerAndUnwrapperImpl([opt_argtype])
            : NO_TYPE_RESTRICTIONS);
}

/**
 * @param {function(T1, T2):?} fn
 * @param {!Type=} opt_argtype1
 * @param {!Type=} opt_argtype2
 * @return {!PrimitiveProcedure}
 * @template T1,T2
 * TODO bl: make the template types mean something
 */
function binary(fn, opt_argtype1, opt_argtype2) {
  const argtypes = [];
  if (goog.isDef(opt_argtype1)) {
    argtypes.push(opt_argtype1);
  }
  if (goog.isDef(opt_argtype2)) {
    argtypes.push(opt_argtype2);
  }
  const typeChecker = argtypes.length === 0
      ? NO_TYPE_RESTRICTIONS
      : new ArgumentTypeCheckerAndUnwrapperImpl(argtypes);
  return new PrimitiveProcedure(fn, EXACTLY_2_ARGS, typeChecker);
}

/**
 * @param {function(T1, T2, T3): ?} fn
 * @param {!Type=} opt_argtype1
 * @param {!Type=} opt_argtype2
 * @param {!Type=} opt_argtype3
 * @return {!PrimitiveProcedure}
 * @template T1,T2,T3
 * TODO bl make the template types mean something
 */
function ternary(fn, opt_argtype1, opt_argtype2, opt_argtype3) {
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
      ? new ArgumentTypeCheckerAndUnwrapperImpl(argtypes)
      : NO_TYPE_RESTRICTIONS;
  return new PrimitiveProcedure(fn, EXACTLY_3_ARGS, typeChecker);
}

/**
 * @param {!Function} fn
 * @param {!Type} typeOfAllArgs
 * @return {!PrimitiveProcedure}
 */
function varargsAtLeast0(fn, typeOfAllArgs) {
  return new PrimitiveProcedure(fn, ANY_NUMBER_OF_ARGS, new AllArgsOfType(typeOfAllArgs));
}

/**
 * @param {!Function} fn
 * @param {!Type} typeOfAllArgs
 * @return {!PrimitiveProcedure}
 */
function varargsAtLeast1(fn, typeOfAllArgs) {
  return new PrimitiveProcedure(fn, AT_LEAST_1_ARG, new AllArgsOfType(typeOfAllArgs));
}

/**
 * @param {!Function} fn
 * @param {number} minArgs
 * @param {number} maxArgs
 * @return {!PrimitiveProcedure}
 */
function varargsRange(fn, minArgs, maxArgs) {
  return new PrimitiveProcedure(fn, new Between(minArgs, maxArgs), NO_TYPE_RESTRICTIONS);
}

/**
 * @param {function(!InputPort, !OutputPort): ?} fn
 * @return {!PrimitiveProcedure}
 */
function nullaryWithCurrentPorts(fn) {
  return new NeedsCurrentPorts(fn, new Exactly(0), NO_TYPE_RESTRICTIONS);
}

/**
 * @param {function(!InputPort, !OutputPort, ?, ?): ?} fn
 * @return {!PrimitiveProcedure}
 */
function binaryWithCurrentPorts(fn) {
  return new NeedsCurrentPorts(fn, new Exactly(2), NO_TYPE_RESTRICTIONS);
}

/**
 * @param {function(!InputPort, !OutputPort, ?): ?} fn
 * @return {!PrimitiveProcedure}
 */
function nullaryOrUnaryWithCurrentPorts(fn) {
  return new NeedsCurrentPorts(fn, new Between(0, 1), NO_TYPE_RESTRICTIONS);
}

/**
 * @param {function(!InputPort, !OutputPort, ?, ?): ?} fn
 * @return {!PrimitiveProcedure}
 */
function unaryOrBinaryWithCurrentPorts(fn) {
  return new NeedsCurrentPorts(fn, new Between(1, 2), NO_TYPE_RESTRICTIONS);
}

/**
 * @param {function(?, !ProcCallLike, !TrampolineHelper): ?}  fn
 * @return {!PrimitiveProcedure}
 */
function unaryWithSpecialEvalLogic(fn) {
  return new HasSpecialEvalLogic(fn, EXACTLY_1_ARG, JUST_UNWRAP_ARGS);
}

/**
 * @param {function(?, ?, !ProcCallLike, !TrampolineHelper): ?} fn
 * @return {!PrimitiveProcedure}
 */
function binaryWithSpecialEvalLogic(fn) {
  return new HasSpecialEvalLogic(fn, EXACTLY_2_ARGS, JUST_UNWRAP_ARGS);
}

/**
 * @param {function(?, ?, ?, !ProcCallLike, !TrampolineHelper): ?} fn
 * @return {!PrimitiveProcedure}
 */
function ternaryWithSpecialEvalLogic(fn) {
  return new HasSpecialEvalLogic(fn, EXACTLY_3_ARGS, JUST_UNWRAP_ARGS);
}

/**
 * @param {number} min
 * @param {!Function} fn
 * @return {!PrimitiveProcedure}
 */
function atLeastNWithSpecialEvalLogic(min, fn) {
  return new HasSpecialEvalLogic(fn, new AtLeast(min), NO_TYPE_RESTRICTIONS);
}

exports.unary = unary;
exports.binary = binary;
exports.ternary = ternary;
exports.varargsAtLeast0 = varargsAtLeast0;
exports.varargsAtLeast1 = varargsAtLeast1;
exports.varargsRange = varargsRange;
exports.nullaryWithCurrentPorts = nullaryWithCurrentPorts;
exports.binaryWithCurrentPorts = binaryWithCurrentPorts;
exports.nullaryOrUnaryWithCurrentPorts = nullaryOrUnaryWithCurrentPorts;
exports.unaryOrBinaryWithCurrentPorts = unaryOrBinaryWithCurrentPorts;
exports.unaryWithSpecialEvalLogic = unaryWithSpecialEvalLogic;
exports.binaryWithSpecialEvalLogic = binaryWithSpecialEvalLogic;
exports.ternaryWithSpecialEvalLogic = ternaryWithSpecialEvalLogic;
exports.atLeastNWithSpecialEvalLogic = atLeastNWithSpecialEvalLogic;

/** @const {!Object<string, !PrimitiveProcedure>} */
const Predicates = {};

Predicates['boolean?'] = unary(node => node instanceof Boolean);
Predicates['char?'] = unary(node => node instanceof Character);
Predicates['input-port?'] = unary(port => InputPort.isImplementedBy(port));
Predicates['null?'] = unary(node => node instanceof List && !node.getFirstChild());
Predicates['number?'] = unary(node => node instanceof Number);
Predicates['output-port?'] = unary(port => OutputPort.isImplementedBy(port));
// 3.2: (pair? '()) => #f
Predicates['pair?'] = unary(node => IPair.isImplementedBy(node) && !!node.getFirstChild());
Predicates['port?'] = unary(port => InputPort.isImplementedBy(port) || OutputPort.isImplementedBy(port));
/* R5RS 6.4: "The procedure call-with-current-continuation
 packages up the current continuation as an "escape procedure"
 and passes it as an argument to proc." Thus a Continuation
 must count as a procedure. */
Predicates['procedure?'] = unary(node => node instanceof Lambda || node instanceof Continuation);
Predicates['string?'] = unary(node => node instanceof String);
Predicates['symbol?'] = unary(node => node instanceof Identifier);
Predicates['vector?'] = unary(node => node instanceof Vector);

exports.Predicates = Predicates;

/**
 * @param {!Datum} arg
 * @return {!Type}
 */
function runtimeType(arg) {
    for (const key in Type.Types) {
        const type = Type.Types[key];
        const predicateName = type.getName() + '?';
        if (predicateName in Predicates
            && !!Predicates[predicateName].fn_.call(null, arg)) {
            return type;
        }
    }
    throw Error.internalInterpreterError("unknown type: " + arg);
}

exports.PrimitiveProcedure = PrimitiveProcedure; // TODO bl necessary?
exports.runtimeType = runtimeType;
