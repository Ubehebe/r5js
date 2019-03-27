import {Boolean} from "../ast/boolean";
import {Character} from "../ast/character";
import {Datum} from "../ast/datum";
import {wrapValue} from "../ast/datum_util";
import {Identifier} from "../ast/identifier";
import {List} from "../ast/list";
import {Number} from "../ast/number";
import {Pair} from "../ast/pair";
import {ProcCallLike, ProcCallResult} from "../ast/proc_call_like";
import {Ref} from "../ast/ref";
import {SimpleDatum} from "../ast/simple_datum";
import {String} from "../ast/string";
import {Type} from "../ast/type";
import * as Types from "../ast/types";
import {Vector} from "../ast/vector";
import {Error} from "../error";
import {InputPort, isInputPort} from "../io/input_port";
import {isOutputPort, OutputPort} from "../io/output_port";
import {Value} from "../value";
import {Continuation} from "./continuation";
import {Environment} from "./environment";
import {argumentTypeError} from "./errors";
import {Lambda} from "./lambda";
import {Procedure} from "./procedure";
import {TrampolineHelper} from "./trampoline_helper";

export interface NumArgChecker {
  checkNumArgs(numArgs: number, nameToShowInErrorMessage: string): void;
}

class Exactly implements NumArgChecker {

  constructor(private readonly numArgs: number) {}

  /** @override */
  checkNumArgs(numArgs: number, nameToShowInErrorMessage: string) {
    if (numArgs !== this.numArgs) {
      throw Error.incorrectNumArgs(nameToShowInErrorMessage, this.numArgs, numArgs);
    }
  }
}

class AtLeast implements NumArgChecker {

  constructor(private readonly min: number) {}

  /** @override */
  checkNumArgs(numArgs: number, nameToShowInErrorMessage: string) {
    if (numArgs < this.min) {
      throw Error.tooFewVarargs(nameToShowInErrorMessage, this.min, numArgs);
    }
  }
}

class Between implements NumArgChecker {

  constructor(
      private readonly minArgs: number,
      private readonly maxArgs: number) {}

  /** @override */
  checkNumArgs(numArgs: number, nameToShowInErrorMessage: string) {
    if (numArgs < this.minArgs) {
      throw Error.tooFewVarargs(nameToShowInErrorMessage, this.minArgs, numArgs);
    }
    if (numArgs > this.maxArgs) {
      throw Error.tooManyVarargs(nameToShowInErrorMessage, this.maxArgs, numArgs);
    }
  }
}

const EXACTLY_1_ARG = new Exactly(1);
const EXACTLY_2_ARGS = new Exactly(2);
const EXACTLY_3_ARGS = new Exactly(3);
const ANY_NUMBER_OF_ARGS = new AtLeast(0);
const AT_LEAST_1_ARG = new AtLeast(1);

export interface ArgumentTypeCheckerAndUnwrapper {
  checkAndUnwrapArgs(args: any[], nameToShowInErrorMessage: string): any[];
}

class ArgumentTypeCheckerAndUnwrapperImpl implements ArgumentTypeCheckerAndUnwrapper {

  constructor(private readonly argtypes: Type[]) {}

  /** @override */
  checkAndUnwrapArgs(args: any[], nameToShowInErrorMessage: string): any[] {
    const unwrappedArgs: any[] = [];
    for (let i = 0; i < this.argtypes.length; ++i) {
      const arg = args[i];
      const expectedType = this.argtypes[i];
      if (!Predicates[expectedType.getName() + '?'].fn.call(null, arg)) {
        const actualType = runtimeType(arg);
        throw argumentTypeError(arg, i, nameToShowInErrorMessage, expectedType, actualType);
      }
      unwrappedArgs.push(arg instanceof SimpleDatum ? arg.unwrap() : arg);
    }
    return unwrappedArgs;
  }
}

class NoTypeChecking implements ArgumentTypeCheckerAndUnwrapper {
  /** @override */
  checkAndUnwrapArgs(args: any[], nameToShowInErrorMessage: string): any[] {
    return args;
  }
}

const NO_TYPE_RESTRICTIONS = new NoTypeChecking();

class AllArgsOfType implements ArgumentTypeCheckerAndUnwrapper {

  constructor(private readonly type: Type) {}

  /** @override */
  checkAndUnwrapArgs(args: any[], nameToShowInErrorMessage: string): any[] {
    const argtype = this.type;
    return args.map((arg, i) => {
      if (!(Predicates[argtype.getName() + '?']).fn.call(null, arg)) {
        throw argumentTypeError(
            arg, i, nameToShowInErrorMessage, argtype, runtimeType(arg));
      }
      return arg instanceof SimpleDatum ? arg.unwrap() : arg;
    });
  }
}

export class PrimitiveProcedure extends Procedure {

  protected debugName = '';

  constructor(
      readonly fn: any /* TODO narrow */,
      protected readonly numArgChecker: NumArgChecker,
      protected readonly typeChecker: ArgumentTypeCheckerAndUnwrapper) {
    super();
    this.numArgChecker = numArgChecker;
    this.typeChecker = typeChecker;
  }

  /**
   * Procedures have no deep need to know their names, as they are only bindings
   * and can change: (set! car cdr). This method exists only to increase
   * the usefulness of error messages thrown from primitive procedures.
   */
  setDebugName(name: string) {
    this.debugName = name;
  }

  /** @override */
  toString() {
    return `<proc:${this.debugName}>`;
  }

  protected call(userArgs: any[], procCallLike: ProcCallLike, trampolineHelper: TrampolineHelper) {
    this.numArgChecker.checkNumArgs(userArgs.length, this.debugName);
    const unwrappedArgs = this.typeChecker.checkAndUnwrapArgs(
        userArgs, this.debugName);
    const ans = this.fn.apply(null, unwrappedArgs);
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
  evaluate(args: Value[], procCallLike: ProcCallLike, resultStruct: ProcCallResult, env: Environment) {
    args = args.map(wrapValue);
    // todo bl document why we're doing this...
    for (let i = 0; i < args.length; ++i) {
      if (args[i] instanceof Ref) {
        args[i] = (args[i] as Ref).deref();
      }
    }
    this.call(args, procCallLike, resultStruct as TrampolineHelper);
  }
}

class NeedsCurrentPorts extends PrimitiveProcedure {
  constructor(fn: any /* TODO narrow */, numArgChecker: NumArgChecker, typeChecker: ArgumentTypeCheckerAndUnwrapper) {
    super(fn, numArgChecker, typeChecker);
  }

  /** @override */
  call(userArgs: any[], procCallLike: ProcCallLike, trampolineHelper: TrampolineHelper) {
    this.numArgChecker.checkNumArgs(userArgs.length, this.debugName);
    const unwrappedArgs = this.typeChecker.checkAndUnwrapArgs(
        userArgs, this.debugName);
    const args: any[] = [].concat(
      trampolineHelper.getInputPort() as any,
      trampolineHelper.getOutputPort() as any,
      unwrappedArgs as any);
    const ans = this.fn.apply(null, args);
    procCallLike.bindResult(ans);
    trampolineHelper.setValue(ans);
    const nextContinuable = procCallLike.getNext();
    if (nextContinuable) {
      trampolineHelper.setNext(nextContinuable);
    }
  }
}

class HasSpecialEvalLogic extends PrimitiveProcedure {
  constructor(fn: any /* TODO */, numArgChecker: NumArgChecker, typeChecker: ArgumentTypeCheckerAndUnwrapper) {
    super(fn, numArgChecker, typeChecker);
  }

  /** @override */
  call(userArgs: any[], procCallLike: ProcCallLike, trampolineHelper: TrampolineHelper) {
    this.numArgChecker.checkNumArgs(userArgs.length, this.debugName);
    const unwrappedArgs = this.typeChecker.checkAndUnwrapArgs(
        userArgs, this.debugName);
    const args = [].concat(unwrappedArgs as any, procCallLike as any, trampolineHelper as any);
    this.fn.apply(null, args);
  }
}

/** TODO bl: make the template type mean something */
export function unary<T>(fn: (arg: T) => any, argtype: Type | undefined = undefined): PrimitiveProcedure {
  return new PrimitiveProcedure(
      fn,
      EXACTLY_1_ARG,
      argtype
          ? new ArgumentTypeCheckerAndUnwrapperImpl([argtype])
          : NO_TYPE_RESTRICTIONS);
}

/** TODO bl: make the template types mean something */
export function binary<T1, T2>(
    fn: (arg1: T1, arg2: T2) => any,
    argtype1: Type | undefined = undefined,
    argtype2: Type | undefined = undefined): PrimitiveProcedure {
  const argtypes: Type[] = [];
  argtype1 && argtypes.push(argtype1);
  argtype2 && argtypes.push(argtype2);
  const typeChecker = argtypes.length === 0
      ? NO_TYPE_RESTRICTIONS
      : new ArgumentTypeCheckerAndUnwrapperImpl(argtypes);
  return new PrimitiveProcedure(fn, EXACTLY_2_ARGS, typeChecker);
}

/** TODO bl make the template types mean something */
export function ternary<T1, T2, T3>(
    fn: (arg1: T1, arg2: T2, arg3: T3) => any,
    argtype1: Type | undefined = undefined,
    argtype2: Type | undefined = undefined,
    argtype3: Type | undefined = undefined): PrimitiveProcedure {
  const argtypes: Type[] = [];
  argtype1 && argtypes.push(argtype1);
  argtype2 && argtypes.push(argtype2);
  argtype3 && argtypes.push(argtype3);
  const typeChecker = argtypes.length
      ? new ArgumentTypeCheckerAndUnwrapperImpl(argtypes)
      : NO_TYPE_RESTRICTIONS;
  return new PrimitiveProcedure(fn, EXACTLY_3_ARGS, typeChecker);
}

export function varargsAtLeast0(fn: any /* TODO */, typeOfAllArgs: Type): PrimitiveProcedure {
  return new PrimitiveProcedure(fn, ANY_NUMBER_OF_ARGS, new AllArgsOfType(typeOfAllArgs));
}

export function varargsAtLeast1(fn: any /* TODO */, typeOfAllArgs: Type): PrimitiveProcedure {
  return new PrimitiveProcedure(fn, AT_LEAST_1_ARG, new AllArgsOfType(typeOfAllArgs));
}

export function varargsRange(fn: any /* TODO */, minArgs: number, maxArgs: number): PrimitiveProcedure {
  return new PrimitiveProcedure(fn, new Between(minArgs, maxArgs), NO_TYPE_RESTRICTIONS);
}

export function nullaryWithCurrentPorts(fn: (i: InputPort, o: OutputPort) => any): PrimitiveProcedure {
  return new NeedsCurrentPorts(fn, new Exactly(0), NO_TYPE_RESTRICTIONS);
}

export function binaryWithCurrentPorts(fn: (i: InputPort, o: OutputPort, arg3: any, arg4: any) => any /* TODO */): PrimitiveProcedure {
  return new NeedsCurrentPorts(fn, new Exactly(2), NO_TYPE_RESTRICTIONS);
}

export function nullaryOrUnaryWithCurrentPorts(fn: (i: InputPort, o: OutputPort, arg3: any) => any /* TODO */): PrimitiveProcedure {
  return new NeedsCurrentPorts(fn, new Between(0, 1), NO_TYPE_RESTRICTIONS);
}

export function unaryOrBinaryWithCurrentPorts(fn: (i: InputPort, o: OutputPort, arg3: any, arg4: any) => any): PrimitiveProcedure {
  return new NeedsCurrentPorts(fn, new Between(1, 2), NO_TYPE_RESTRICTIONS);
}

export function unaryWithSpecialEvalLogic(fn: (arg1: any, arg2: ProcCallLike, arg3: TrampolineHelper) => any): PrimitiveProcedure {
  return new HasSpecialEvalLogic(fn, EXACTLY_1_ARG, NO_TYPE_RESTRICTIONS);
}

export function binaryWithSpecialEvalLogic(fn: (arg1: any, arg2: any, arg3: ProcCallLike, arg4: TrampolineHelper) => any): PrimitiveProcedure {
  return new HasSpecialEvalLogic(fn, EXACTLY_2_ARGS, NO_TYPE_RESTRICTIONS);
}

export function ternaryWithSpecialEvalLogic(fn: (arg1: any, arg2: any, arg3: any, arg4: ProcCallLike, arg5: TrampolineHelper) => any): PrimitiveProcedure {
  return new HasSpecialEvalLogic(fn, EXACTLY_3_ARGS, NO_TYPE_RESTRICTIONS);
}

export function atLeastNWithSpecialEvalLogic(min: number, fn: any /* TODO */): PrimitiveProcedure {
  return new HasSpecialEvalLogic(fn, new AtLeast(min), NO_TYPE_RESTRICTIONS);
}

const Predicates: { [key: string]: PrimitiveProcedure } = {};

Predicates['boolean?'] = unary(node => node instanceof Boolean);
Predicates['char?'] = unary(node => node instanceof Character);
Predicates['input-port?'] = unary(port => isInputPort(port));
Predicates['null?'] = unary(node => node instanceof List && !node.getFirstChild());
Predicates['number?'] = unary(node => node instanceof Number);
Predicates['output-port?'] = unary(port => isOutputPort(port));
// 3.2: (pair? '()) => #f
Predicates['pair?'] = unary(node => node instanceof Pair && !!node.getFirstChild());
Predicates['port?'] = unary(port => isInputPort(port) || isOutputPort(port));
/* R5RS 6.4: "The procedure call-with-current-continuation
 packages up the current continuation as an "escape procedure"
 and passes it as an argument to proc." Thus a Continuation
 must count as a procedure. */
Predicates['procedure?'] = unary(node => node instanceof Lambda || node instanceof Continuation);
Predicates['string?'] = unary(node => node instanceof String);
Predicates['symbol?'] = unary(node => node instanceof Identifier);
Predicates['vector?'] = unary(node => node instanceof Vector);

export function runtimeType(arg: Datum): Type {
  const types = (Types as any); // TODO iterating over imports is weird
  for (const typeName in types) {
    const type = types[typeName];
    const predicateName = type.getName() + '?';
    if (predicateName in Predicates
        && !!Predicates[predicateName].fn.call(null, arg)) {
      return type;
    }
  }
  throw Error.internalInterpreterError(`unknown type: ${arg}`);
}

export function installPredicates(registry: { [key: string]: PrimitiveProcedure }) {
  for (const name in Predicates) {
    registry[name] = Predicates[name];
  }
}