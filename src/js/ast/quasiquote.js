goog.provide('r5js.ast.Quasiquote');


goog.require('r5js.ContinuableHelper');
goog.require('r5js.ProcCallLike');
goog.require('r5js.ast.CompoundDatum');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.Unquote');
goog.require('r5js.ast.UnquoteSplicing');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
// TODO bl circular dependency goog.require('r5js.IdShim');



/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.ast.CompoundDatum}
 * @struct
 * @constructor
 */
r5js.ast.Quasiquote = function(firstChild) {
  goog.base(this);
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


/** @override */
r5js.ast.Quasiquote.prototype.stringForOutputMode = function(outputMode) {
  var children = this.mapChildren(function(child) {
    return child.stringForOutputMode(outputMode);
  });
  return r5js.parse.Terminals.BACKTICK + children.join(' ');
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

  var newCalls = new r5js.ContinuableHelper();

  var qqLevel = this.qqLevel;

  this.replaceChildren(
      function(node) {
        return (node instanceof r5js.ast.Unquote ||
                node instanceof r5js.ast.UnquoteSplicing) &&
            node.getQQLevel() === qqLevel;
      },
      function(node) {
        node = /** @type {!r5js.ast.CompoundDatum} */ (node); // TODO bl
        var asContinuable = (/** @type {!r5js.ProcCallLike} */ (parserProvider(
            /** @type {!r5js.Datum} */(node.getFirstChild())).
                parse(r5js.parse.Nonterminals.EXPRESSION).
                desugar(env, true)));
        var continuation = r5js.ProcCallLike.getLast(asContinuable).
            getContinuation();
        /* Throw out the last result name and replace it with another
             identifier (also illegal in Scheme) that will let us know if it's
             unquotation or unquotation with splicing. */
        var name = (node instanceof r5js.ast.Unquote ?
                r5js.parse.Terminals.COMMA :
                r5js.parse.Terminals.COMMA_AT) + '' + goog.getUid(new Object());
        continuation.setLastResultName(name);
        newCalls.appendProcCallLike(asContinuable);
        return new r5js.ast.Identifier(name);
      });

  var newDatum = new r5js.ast.Quote(this.getFirstChild());

  newCalls.appendProcCallLike(new r5js.IdShim(newDatum, cpsName));
  var ans = newCalls.toContinuable();
  ans.setStartingEnv(env);
  return ans;
};


/** @override */
r5js.ast.Quasiquote.prototype.setQuasiquotationLevel = function(qqLevel) {
  this.qqLevel = qqLevel + 1;
  return goog.base(this, 'setQuasiquotationLevel', this.qqLevel);
};
