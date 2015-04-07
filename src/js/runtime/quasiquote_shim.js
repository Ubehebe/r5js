goog.provide('r5js.QuasiquoteShim');

goog.require('r5js.ProcCallLike');

/**
 * TODO bl the purpose of this class is unclear.
 * @param {!r5js.ast.Quasiquote} payload
 * @param {string=} opt_continuationName Optional name of the continuation.
 * @extends {r5js.ProcCallLike}
 * @struct
 * @constructor
 */
r5js.QuasiquoteShim = function(payload, opt_continuationName) {
    r5js.QuasiquoteShim.base(this, 'constructor', opt_continuationName);
    /** @const @private */ this.firstOperand_ = payload;
};
goog.inherits(r5js.QuasiquoteShim, r5js.ProcCallLike);

/** @override */
r5js.QuasiquoteShim.prototype.evalAndAdvance = function(resultStruct, env, parserProvider) {
    const next = this.tryQuasiquote_(this.firstOperand_, parserProvider);
    if (next) {
        resultStruct.setNext(next);
    }
};

/**
 * @param {!r5js.ast.Quasiquote} quasiquote
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider
 * @return {r5js.ProcCallLike}
 * @private
 */
r5js.QuasiquoteShim.prototype.tryQuasiquote_ = function(quasiquote, parserProvider) {
    const continuable = quasiquote.processQuasiquote(
        /** @type {!r5js.IEnvironment} */ (this.getEnv()),
        this.getResultName(),
        parserProvider);
    const next = this.getNext();
    if (next) {
        r5js.ProcCallLike.appendProcCallLike(continuable, next);
    }
    return continuable;
};
