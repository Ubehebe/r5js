goog.module('r5js.ProcCallLike');

const {ProcCallResult} = goog.require('r5js.ProcCallResult');

class ProcCallLike {
    /** @param {string=} lastResultName */
    constructor(lastResultName=undefined) {
        /** @private */ this.resultName_ = lastResultName ||
        ('@' /* TODO bl document */ + goog.getUid(this));
        /** @private {?ProcCallLike} */ this.next_ = null;
        /** @private {?IEnvironment} */ this.env_ = null;
    }

    /**
     * @param {!ProcCallResult} resultStruct
     * @param {!IEnvironment} env
     * @param {!Function} parserProvider Function
     * that will return a new Parser for the given Datum when called.
     */
    evalAndAdvance(resultStruct, env, parserProvider) {
    }

    /** @return {string} */
    getResultName() {
        return this.resultName_;
    }

    /**
     * @param {string} resultName
     * TODO bl remove.
     */
    setResultName(resultName) {
        this.resultName_ = resultName;
    }

    /** @param {!IEnvironment} env */
    setStartingEnv(env) {
        this.env_ = env;
    }

    /** @return {?IEnvironment} */
    getEnv() {
        return this.env_;
    }

    /** Clears the current environment. TODO bl not well understood. */
    clearEnv() {
        this.env_ = null;
    }

    /** @return {?ProcCallLike} */
    getNext() {
        return this.next_;
    }

    /** @param {!ProcCallLike} next */
    setNext(next) {
        this.next_ = next;
    }

    /** @param {!Value} val */
    bindResult(val) {
        /* If the next procedure call already has an environment,
         bind the result there. Otherwise, bind it in the current
         environment; it will be carried forward by the EnvBuffer. */
        const envToUse = (this.next_ && this.next_.getEnv()) || this.env_;
        envToUse.addBinding(this.resultName_, val);
    }
}

/**
 * @param {!ProcCallLike} procCallLike
 * @return {!ProcCallLike}
 */
function getLastProcCallLike(procCallLike) {
    const maybeNext = procCallLike.getNext();
    return maybeNext ? getLastProcCallLike(maybeNext) : procCallLike;
}

/**
 * @param {!ProcCallLike} procCallLike
 * @param {!ProcCallLike} next The next continuable.
 */
function appendProcCallLike(procCallLike, next) {
    getLastProcCallLike(procCallLike).setNext(next);
}

exports = {ProcCallLike, getLastProcCallLike, appendProcCallLike};