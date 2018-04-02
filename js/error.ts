
export class Error {
    constructor(private readonly type: ErrorType, private readonly msg: string) {}

    equals(other: Error) {
        return this.type === other.type; // TODO bl improve
    }

    static unboundVariable(name: string) {
        return new Error(
            ErrorType.UNBOUND_VARIABLE, `${name} is not defined`);
    }

    static tooFewVarargs(name: string, minNumArgs: number, actualNumArgs: number) {
        return new Error(
            ErrorType.TOO_FEW_VARARGS,
            `${name}: want >= ${minNumArgs} args, got ${actualNumArgs}`);
    }

    static tooManyVarargs(name: string, maxNumArgs: number, actualNumArgs: number) {
        return new Error(
            ErrorType.TOO_MANY_VARARGS,
            `${name}: want <= ${maxNumArgs} args, got ${actualNumArgs}`);
    }

    static incorrectNumArgs(name: string, expectedNumArgs: number, actualNumArgs: number) {
        return new Error(
            ErrorType.INCORRECT_NUM_ARGS,
            `${name}: want ${expectedNumArgs} args, got ${actualNumArgs}`);
    }

    static internalInterpreterError(msg: string) {
        return new Error(ErrorType.INTERNAL_INTERPRETER_ERROR, msg);
    }

    static macro(keyword: string, msg: string) {
        return new Error(ErrorType.MACRO, `macro ${keyword}: ${msg}`);
    }

    static unimplementedOption(what: string) {
        return new Error(ErrorType.UNIMPLEMENTED_OPTION, `unimplemented: ${what}`);
    }

    static quasiquote(what: string) {
        return new Error(ErrorType.QUASIQUOTE, what);
    }

    static illegalEmptyApplication(what: string) {
        return new Error(ErrorType.ILLEGAL_EMPTY_APPLICATION, what);
    }

    static parse(what: any) {
        return new Error(ErrorType.PARSE, `parse error: ${what}`);
    }

    static immutable(what: string) {
        return new Error(ErrorType.IMMUTABLE, what);
    }

    static scan(what: string) {
        return new Error(ErrorType.SCAN, `scan error: ${what}`);
    }
}

export enum ErrorType {
    ARGUMENT_TYPE_ERROR = 'argument type error',
    ILLEGAL_EMPTY_APPLICATION = 'illegal empty application',
    IMMUTABLE = 'immutable error',
    INCORRECT_NUM_ARGS = 'incorrect number of args',
    INTERNAL_INTERPRETER_ERROR = 'internal interpreter error',
    MACRO = 'macro error',
    NOT_A_PROCEDURE = 'not a procedure',
    PARSE = 'parse error',
    READ = 'read error',
    QUASIQUOTE = 'quasiquote error',
    SCAN = 'scan error',
    TOO_FEW_VARARGS = 'too few varargs',
    TOO_MANY_VARARGS = 'too many varargs',
    UNBOUND_VARIABLE = 'unbound variable',
    UNIMPLEMENTED_OPTION = 'unimplemented option'
}