goog.provide('r5js.ast.Quasiquote');


goog.require('r5js.ContinuableHelper');
goog.require('r5js.Datum');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.Unquote');
goog.require('r5js.ast.UnquoteSplicing');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');



/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.ast.Quasiquote = function(firstChild) {
  goog.base(this);
  this.setType(r5js.parse.Terminals.BACKTICK); // TODO bl remove
  if (firstChild) {
    this.setFirstChild(firstChild);
  }
};
goog.inherits(r5js.ast.Quasiquote, r5js.Datum);


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
 * @return {*} TODO bl.
 * @suppress {const} for the assignment to continuation.lastResultName,
 * which may indicate a bug. TODO bl investigate.
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
        var asContinuable = /** @type {!r5js.Continuable} */ (parserProvider(
            /** @type {!r5js.Datum} */(node.getFirstChild())).
                parse(r5js.parse.Nonterminals.EXPRESSION).
                desugar(env, true));
        var continuation = asContinuable.getLastContinuable().continuation;
        /* Throw out the last result name and replace it with another
             identifier (also illegal in Scheme) that will let us know if it's
             unquotation or unquotation with splicing. */
        continuation.lastResultName = (node instanceof r5js.ast.Unquote ?
                r5js.parse.Terminals.COMMA :
                r5js.parse.Terminals.COMMA_AT) + '' + goog.getUid(new Object());
        newCalls.appendContinuable(asContinuable);
        return new r5js.ast.Identifier(continuation.lastResultName);
      });

  var newDatum = new r5js.ast.Quote(this.getFirstChild());

  newCalls.appendContinuable(newIdShim(newDatum, cpsName));
  var ans = newCalls.toContinuable();
  return ans && ans.setStartingEnv(env);
};


/** @override */
r5js.ast.Quasiquote.prototype.setQuasiquotationLevel = function(qqLevel) {
  this.qqLevel = qqLevel + 1;
  return goog.base(this, 'setQuasiquotationLevel', this.qqLevel);
};
