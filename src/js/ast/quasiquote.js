goog.provide('r5js.ast.Quasiquote');

goog.require('r5js.ContinuableHelper');
goog.require('r5js.ProcCallLike');
goog.require('r5js.ast.CompoundDatum');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.Quote');
goog.require('r5js.ast.Unquote');
goog.require('r5js.ast.UnquoteSplicing');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');

/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.ast.CompoundDatum}
 * @struct
 * @constructor
 */
r5js.ast.Quasiquote = function(firstChild) {
  r5js.ast.Quasiquote.base(this, 'constructor');
  if (firstChild) {
    this.setFirstChild(firstChild);
  }
};
goog.inherits(r5js.ast.Quasiquote, r5js.ast.CompoundDatum);


/**
 * TODO bl: not sure this is the right thing to do for quasiquotes.
 * We can't just "unescape" the quasiquotations. Example:
 *
 * (equal? '(a `(b ,(+ 1 2))) '(a `(b ,(+ 1 2))))
 *
 * This will eventually call
 *
 * (eqv? `(b ,(+ 1 2)) `(b ,(+ 1 2)))
 *
 * From this procedure call, it looks as if we should unescape the quasiquotes,
 * but that's incorrect; we've lost the surrounding quotation level.
 * It may be possible to figure out what to do based on the qqLevels,
 * but it's been a while since I've looked at that subsystem.
 *
 * @override
 */
r5js.ast.Quasiquote.prototype.eqv = function(other) {
  return this.isEqual(/** @type {!r5js.ast.CompoundDatum} */ (other));
};


/**
 * Example: `(1 ,(+ 2 3)) should desugar as (+ 2 3 [_0 (id (1 _0) [_2 ...])])
 * @param {!r5js.IEnvironment} env TODO bl.
 * @param {string} cpsName TODO bl.
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 * @return {!r5js.ProcCallLike}
 */
r5js.ast.Quasiquote.prototype.processQuasiquote = function(
    env, cpsName, parserProvider) {

  const newCalls = new r5js.ContinuableHelper();

  const qqLevel = this.qqLevel;

  this.replaceChildren(
      function(node) {
        return (node instanceof r5js.ast.Unquote ||
                node instanceof r5js.ast.UnquoteSplicing) &&
            node.getQQLevel() === qqLevel;
      },
      function(node) {
        node = /** @type {!r5js.ast.CompoundDatum} */ (node); // TODO bl
        const asContinuable = (/** @type {!r5js.ProcCallLike} */ (parserProvider(
            /** @type {!r5js.Datum} */(node.getFirstChild())).
                parse(r5js.parse.Nonterminals.EXPRESSION).
                desugar(env, true)));
        /* Throw out the last result name and replace it with another
             identifier (also illegal in Scheme) that will let us know if it's
             unquotation or unquotation with splicing. */
        const name = (node instanceof r5js.ast.Unquote
                ? r5js.parse.Terminals.COMMA
                : r5js.parse.Terminals.COMMA_AT) + '' + goog.getUid(new Object());
        const last = r5js.ProcCallLike.getLast(asContinuable);
        last.setResultName(name);
        newCalls.appendProcCallLike(asContinuable);
        return new r5js.ast.Identifier(name);
      });

  const newDatum = new r5js.ast.Quote(this.getFirstChild());

  newCalls.appendProcCallLike(newDatum.toProcCallLike());
  return /** @type {!r5js.ProcCallLike} */ (newCalls.toContinuable());
};


/** @override */
r5js.ast.Quasiquote.prototype.setQuasiquotationLevel = function(qqLevel) {
  this.qqLevel = qqLevel + 1;
  return r5js.ast.Quasiquote.base(this, 'setQuasiquotationLevel', this.qqLevel);
};

/** @override */
r5js.ast.Quasiquote.prototype.toProcCallLike = function() {
    return new r5js.QuasiquoteShim_(this);
};

/**
 * TODO bl the purpose of this class is unclear.
 * @param {!r5js.ast.Quasiquote} payload
 * @param {string=} opt_continuationName Optional name of the continuation.
 * @extends {r5js.ProcCallLike}
 * @struct
 * @constructor
 * @private
 */
r5js.QuasiquoteShim_ = function(payload, opt_continuationName) {
    r5js.QuasiquoteShim_.base(this, 'constructor', opt_continuationName);
    /** @const @private */ this.firstOperand_ = payload;
};
goog.inherits(r5js.QuasiquoteShim_, r5js.ProcCallLike);

/** @override */
r5js.QuasiquoteShim_.prototype.evalAndAdvance = function(resultStruct, env, parserProvider) {
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
r5js.QuasiquoteShim_.prototype.tryQuasiquote_ = function(quasiquote, parserProvider) {
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
