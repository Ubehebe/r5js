goog.module('r5js.RenameUtil');

const {Terminals} = goog.require('r5js.parse.Terminals');

/** Not a valid identifier prefix so we can easily tell these apart. */
const CPS_PREFIX = '@';

/** @return {string} */
function newCpsName() {
    /* TODO bl: goog.getUid requires an object parameter, so this method
     creates a throwaway object. Requiring this function to take an object
     parameter could reduce garbage. */
    return CPS_PREFIX + goog.getUid(new Object());
}

/**
 * See comments at the top of Parser.
 * @param {string} name identifier name to check.
 * @return {boolean} True iff the given name is parser-sensitive.
 */
function isParserSensitiveId(name) {
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

exports = {
    CPS_PREFIX,
    isParserSensitiveId,
    newCpsName,
};