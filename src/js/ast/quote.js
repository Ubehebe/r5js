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

goog.provide('r5js.ast.Quote');


goog.require('r5js.IPair');
goog.require('r5js.ast.CompoundDatum');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.List');
goog.require('r5js.parse.Terminals');
goog.require('r5js.QuoteShim');



/**
 * @param {r5js.Datum} firstChild
 * @implements {r5js.IPair}
 * @extends {r5js.ast.CompoundDatum}
 * @struct
 * @constructor
 */
r5js.ast.Quote = function(firstChild) {
  r5js.ast.Quote.base(this, 'constructor');
  if (firstChild) {
    this.setFirstChild(firstChild.setImmutable());
  }
};
goog.inherits(r5js.ast.Quote, r5js.ast.CompoundDatum);
r5js.IPair.addImplementation(r5js.ast.Quote);


/** @const @private {!r5js.runtime.Value} */
r5js.ast.Quote.CAR_ = new r5js.ast.Identifier(r5js.parse.Terminals.QUOTE);


/** @override */
r5js.ast.Quote.prototype.car = function() {
  return r5js.ast.Quote.CAR_;
};


/** @override */
r5js.ast.Quote.prototype.cdr = function() {
  return new r5js.ast.List(this.getFirstChild());
};


/** @override */
r5js.ast.Quote.prototype.fixParserSensitiveIds = goog.nullFunction;

/** @override */
r5js.ast.Quote.prototype.toProcCallLike = function() {
    return new r5js.QuoteShim(this);
};
