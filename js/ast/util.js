goog.module('r5js.ast.util');

const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const {Datum, SimpleDatum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const Identifier = goog.require('r5js.ast.Identifier');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const {List} = goog.require('r5js.ast.List');
const {Nonterminals} = require('/js/parse/nonterminals_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');
const {Terminals} = require('/js/parse/terminals_collect_es6_sources.es6/node_modules/__main__/js/parse/terminals');

/**
 * Munges definitions to get them in a form suitable for let-type bindings.
 * Example:
 * (define (foo x y z) ...) => (foo (lambda (x y z) ...))
 * @param {!CompoundDatum} datum Datum to extract the definition from.
 * TODO bl: you can't extract a definition from an arbitrary datum.
 * Make more strongly typed.
 * @return {!CompoundDatum} A datum representing the given datum's definition.
 */
function extractDefinition(datum) {
  let variable = datum.at(Nonterminals.VARIABLE);
  if (variable) {
    const expr = datum.at(Nonterminals.EXPRESSION);
    variable.setNextSibling(null); // TODO bl
    return new SiblingBuffer()
        .appendSibling(variable)
        .appendSibling(/** @type {!Datum} */(expr))
        .toList(List);
  } else {
    const formalsList = /** @type {!CompoundDatum} */ (datum.getFirstChild().getNextSibling());
    variable = formalsList.getFirstChild();
    const bodyStart = /** @type {!Datum} */ (formalsList.getNextSibling());
    formalsList.setFirstChild(/** @type {!Datum} */ (variable.getNextSibling()));
    const lambda = prepareLambdaForDefinition(bodyStart, formalsList);
    variable.setNextSibling(null); // TODO bl
    return new SiblingBuffer()
        .appendSibling(variable)
        .appendSibling(lambda)
        .toList(List);
  }
}

/**
 * @param {!Datum} bodyStart
 * @param {!CompoundDatum} formalsList
 * @return {!Datum}
 */
function prepareLambdaForDefinition(bodyStart, formalsList) {
  const buffer = new SiblingBuffer();
  buffer.appendSibling(new Identifier(Terminals.LAMBDA));
  if (formalsList.isImproperList()
      && !formalsList.getFirstChild().getNextSibling()) {
    buffer.appendSibling(new Identifier(
        (/** @type {!SimpleDatum<string>} */ (formalsList.getFirstChild())).getPayload()));
  } else {
    formalsList.setNextSibling(null);
    buffer.appendSibling(formalsList);
  }
  buffer.appendSibling(bodyStart);
  return buffer.toList(List);
}

exports = {extractDefinition};