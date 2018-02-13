import {Terminals} from './terminals';

/** Not a valid identifier prefix so we can easily tell these apart. */
export const CPS_PREFIX = '@';

let cpsCounter: number = 0;

export function newCpsName(): string {
    return CPS_PREFIX + cpsCounter++;
}

/** See comments at the top of Parser. */
export function isParserSensitiveId(name:string): boolean {
    switch (name) {
        case Terminals.BEGIN:
        case Terminals.DEFINE:
        case Terminals.DEFINE_SYNTAX:
        case Terminals.IF:
        case Terminals.LAMBDA:
        case Terminals.LET_SYNTAX:
        case Terminals.LETREC_SYNTAX:
        case Terminals.QUASIQUOTE:
        case Terminals.QUOTE:
        case Terminals.SET:
        case Terminals.UNQUOTE:
        case Terminals.UNQUOTE_SPLICING:
            return true;
        default:
            return false;
    }
}