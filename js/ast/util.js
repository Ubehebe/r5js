goog.module('r5js.ast.util');

const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const Identifier = goog.require('r5js.ast.Identifier');
const List = goog.require('r5js.ast.List');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const {Nonterminals} = goog.require('r5js.parse.Nonterminals');
const {Terminals} = goog.require('r5js.parse.Terminals');

/**
 * Munges definitions to get them in a form suitable for let-type bindings.
 * Example:
 * (define (foo x y z) ...) => (foo (lambda (x y z) ...))
 * @param {!CompoundDatum} datum Datum to extract the definition from.
 * TODO bl: you can't extract a definition from an arbitrary datum.
 * Make more strongly typed.
 * @return {!CompoundDatum} A datum representing the given datum's definition.
 * @suppress {checkTypes} for setNextSibling(null)
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
    const formalsList = datum.getFirstChild().getNextSibling();
    variable = formalsList.getFirstChild();
    const bodyStart = formalsList.getNextSibling();
    formalsList.setFirstChild(formalsList.getFirstChild().getNextSibling());
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
 * @param {!Datum} formalsList
 * @return {!Datum}
 * @suppress {checkTypes} for setNextSibling(null)
 */
function prepareLambdaForDefinition(bodyStart, formalsList) {
  const buffer = new SiblingBuffer();
  buffer.appendSibling(new Identifier(Terminals.LAMBDA));
  if (formalsList.isImproperList()
      && !formalsList.getFirstChild().getNextSibling()) {
    buffer.appendSibling(new Identifier(
        /** @type {string} */ (formalsList.getFirstChild().getPayload())));
  } else {
    formalsList.setNextSibling(null);
    buffer.appendSibling(formalsList);
  }
  buffer.appendSibling(bodyStart);
  return buffer.toList(List);
}

exports = {extractDefinition};