import {Type} from "../ast/type";
import {ErrorType} from "../error";
import {Error} from "../error";

/** @param argIndex The position of the argument in the argument list (zero-indexed). */
export function argumentTypeError(
    arg: Value,
    argIndex: number,
    procName: string,
    expectedType: Type,
    actualType: Type): Error {
  return new Error(
      ErrorType.ARGUMENT_TYPE_ERROR,
      `${procName}: argument ${argIndex}: want ${expectedType}', got ${actualType}`);
}

/** @param name Error message. */
export function notAProcedure(name: string, actualType: Type | undefined = undefined): Error {
  return new Error(ErrorType.NOT_A_PROCEDURE, name, /*actualType TODO re-enable */);
}