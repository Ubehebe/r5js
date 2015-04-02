goog.provide('r5js.ProcCallLike');

r5js.ProcCallLike = class {
    /** @param {string=} opt_lastResultName */
    constructor(opt_lastResultName) {
        /** @private */ this.resultName_ = opt_lastResultName ||
        ('@' /* TODO bl document */ + goog.getUid(this));
        /** @private {r5js.ProcCallLike} */ this.next_ = null;
        /** @private {r5js.IEnvironment} */ this.env_ = null;
    }

    /**
     * @param {!r5js.TrampolineHelper} trampolineHelper
     * @param {!r5js.IEnvironment} env
     * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
     * that will return a new Parser for the given Datum when called.
     */
    evalAndAdvance(trampolineHelper, env, parserProvider) {
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

    /** @param {!r5js.IEnvironment} env */
    setStartingEnv(env) {
        this.env_ = env;
    }

    /** @return {r5js.IEnvironment} */
    getEnv() {
        return this.env_;
    }

    /** Clears the current environment. TODO bl not well understood. */
    clearEnv() {
        this.env_ = null;
    }

    /** @return {r5js.ProcCallLike} */
    getNext() {
        return this.next_;
    }

    /** @param {!r5js.ProcCallLike} next */
    setNext(next) {
        this.next_ = next;
    }

    /** @param {!r5js.runtime.Value} val */
    bindResult(val) {
        /* If the next procedure call already has an environment,
         bind the result there. Otherwise, bind it in the current
         environment; it will be carried forward by the EnvBuffer. */
        var envToUse = (this.next_ && this.next_.getEnv()) || this.env_;
        envToUse.addBinding(this.resultName_, val);
    }

    /**
     * @param {!r5js.ProcCallLike} procCallLike
     * @return {!r5js.ProcCallLike}
     */
    static getLast(procCallLike) {
        var maybeNext = procCallLike.getNext();
        return maybeNext ? r5js.ProcCallLike.getLast(maybeNext) : procCallLike;
    }

    /**
     * @param {!r5js.ProcCallLike} procCallLike
     * @param {!r5js.ProcCallLike} next The next continuable.
     */
    static appendProcCallLike(procCallLike, next) {
        // TODO bl: Closure Compiler es6 bug? Seems like getLast shouldn't have to be qualified.
        r5js.ProcCallLike.getLast(procCallLike).setNext(next);
    }
};