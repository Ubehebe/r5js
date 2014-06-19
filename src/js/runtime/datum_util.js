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

goog.provide('r5js.datumutil');


goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.Boolean');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Number');
goog.require('r5js.error');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');


/**
 * Munges definitions to get them in a form suitable for let-type bindings.
 * Example:
 * (define (foo x y z) ...) => (foo (lambda (x y z) ...))
 * @param {!r5js.Datum} datum Datum to extract the definition from.
 * TODO bl: you can't extract a definition from an arbitrary datum.
 * Make more strongly typed.
 * @return {!r5js.Datum} A datum representing the given datum's definition.
 * @suppress {checkTypes} for setNextSibling(null)
 */
r5js.datumutil.extractDefinition = function(datum) {
  var variable = datum.at(r5js.parse.Nonterminals.VARIABLE);
  if (variable) {
    var expr = datum.at(r5js.parse.Nonterminals.EXPRESSION);
    variable.setNextSibling(null); // TODO bl
    return new r5js.SiblingBuffer().
        appendSibling(variable).
            appendSibling(/** @type {!r5js.Datum} */(expr)).
            toList(r5js.ast.List);
  } else {
    var formalsList = datum.getFirstChild().getNextSibling();
    variable = formalsList.getFirstChild();
    var bodyStart = formalsList.getNextSibling();
    formalsList.setFirstChild(formalsList.getFirstChild().getNextSibling());
    var lambda = r5js.datumutil.prepareLambdaForDefinition_(
        bodyStart, formalsList);
    variable.setNextSibling(null); // TODO bl
    return new r5js.SiblingBuffer().
        appendSibling(variable).
            appendSibling(lambda).
            toList(r5js.ast.List);
  }
};


/**
 * @param {!r5js.Datum} bodyStart
 * @param {!r5js.Datum} formalsList
 * @return {!r5js.Datum}
 * @private
 * @suppress {checkTypes} for setNextSibling(null)
 */
r5js.datumutil.prepareLambdaForDefinition_ = function(bodyStart, formalsList) {
  var buffer = new r5js.SiblingBuffer();
  buffer.appendSibling(new r5js.ast.Identifier(r5js.parse.Terminals.LAMBDA));
  if (formalsList.isImproperList() &&
      !formalsList.getFirstChild().getNextSibling()) {
    buffer.appendSibling(new r5js.ast.Identifier(
        /** @type {string} */ (formalsList.getFirstChild().getPayload())));
  } else {
    formalsList.setNextSibling(null);
    buffer.appendSibling(formalsList);
  }
  buffer.appendSibling(bodyStart);
  return buffer.toList(r5js.ast.List);
};


/**
 * Environments bind names to values, and this implementation represents
 * values idiomatically in JavaScript. For example, the Scheme value #f
 * is represented in environment bindings by the JavaScript value false.
 *
 * However, in a few cases it is useful to retrieve a value from an environment
 * and then wrap it in an AST node. For example, during evaluation of a varargs
 * procedure call: ((lambda (x . xs) xs) 1 2 3), the "rest args" are rolled up
 * into a list and bound to the identifier xs. The list is a
 * {@link r5js.ast.List}, and its elements must be {@link r5js.Datum} instances,
 * not JavaScript numbers.
 * @see {r5js.VarargsUserDefinedProcedure#bindArgs}
 *
 * @param {!r5js.runtime.Value} result The value to potentially wrap.
 * @return {!r5js.Datum} The value, wrapped in a {@link r5js.Datum}
 * if necessary.
 */
r5js.datumutil.wrapValue = function(result) {
  switch (typeof result) {
    case 'boolean':
      return new r5js.ast.Boolean(result);
    case 'number':
      return new r5js.ast.Number(result);
    case 'object':
      return /** @type {!r5js.Datum} */ (result);
    case 'string':
      return new r5js.ast.Identifier(result);
    default:
      throw r5js.error.internalInterpreterError(
          'cannot deduce type from value ' +
          result +
          ': noninjective mapping from values to types');
  }
};
