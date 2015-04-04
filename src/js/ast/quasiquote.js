/* Copyright 2011-2014 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

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
// TODO bl circular dependency goog.require('r5js.IdShim');



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

  newCalls.appendProcCallLike(new r5js.IdShim(newDatum, cpsName));
  return /** @type {!r5js.ProcCallLike} */ (newCalls.toContinuable());
};


/** @override */
r5js.ast.Quasiquote.prototype.setQuasiquotationLevel = function(qqLevel) {
  this.qqLevel = qqLevel + 1;
  return r5js.ast.Quasiquote.base(this, 'setQuasiquotationLevel', this.qqLevel);
};
