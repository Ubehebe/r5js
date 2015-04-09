goog.module('r5js.RenameUtil');

/** Not a valid identifier prefix so we can easily tell these apart. */
const CPS_PREFIX = '@';

/** @const */
const PROC_PREFIX = 'proc';

/** @return {string} */
function newAnonymousLambdaName() {
    /* TODO bl: goog.getUid requires an object parameter, so this method
     creates a throwaway object. Requiring this function to take an object
     parameter could reduce garbage. */
    return PROC_PREFIX + goog.getUid(new Object());
}


/** @return {string} */
function newCpsName() {
    /* TODO bl: goog.getUid requires an object parameter, so this method
     creates a throwaway object. Requiring this function to take an object
     parameter could reduce garbage. */
    return CPS_PREFIX + goog.getUid(new Object());
}

exports.CPS_PREFIX = CPS_PREFIX;
exports.newAnonymousLambdaName = newAnonymousLambdaName;
exports.newCpsName = newCpsName;
