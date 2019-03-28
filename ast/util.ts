import {EXPRESSION, VARIABLE} from '../parse/nonterminals';
import {LAMBDA} from '../parse/terminals';
import {CompoundDatum} from './compound_datum';
import {Datum} from './datum';
import {Identifier} from './identifier';
import {List} from './list';
import {SiblingBuffer} from './sibling_buffer';

/**
 * Munges definitions to get them in a form suitable for let-type bindings.
 * Example:
 * (define (foo x y z) ...) => (foo (lambda (x y z) ...))
 * @param datum Datum to extract the definition from.
 * TODO bl: you can't extract a definition from an arbitrary datum.
 * Make more strongly typed.
 * @return A datum representing the given datum's definition.
 */
export function extractDefinition(datum: CompoundDatum): CompoundDatum {
  let variable = datum.at(VARIABLE);
  if (variable) {
    const expr = datum.at(EXPRESSION);
    variable.setNextSibling(null); // TODO bl
    return new SiblingBuffer()
        .appendSibling(variable)
        .appendSibling(expr as Datum)
        .toList(List);
  } else {
    const formalsList = datum.getFirstChild()!.getNextSibling() as CompoundDatum;
    variable = formalsList.getFirstChild()!;
    const bodyStart = formalsList.getNextSibling()!;
    formalsList.setFirstChild(variable.getNextSibling()!);
    const lambda = prepareLambdaForDefinition(bodyStart, formalsList);
    variable.setNextSibling(null); // TODO bl
    return new SiblingBuffer()
        .appendSibling(variable)
        .appendSibling(lambda)
        .toList(List);
  }
}

function prepareLambdaForDefinition(bodyStart: Datum, formalsList: CompoundDatum): Datum {
  const buffer = new SiblingBuffer();
  buffer.appendSibling(new Identifier(LAMBDA));
  if (formalsList.isImproperList()
      && !formalsList.getFirstChild()!.getNextSibling()) {
    buffer.appendSibling(new Identifier(
        (formalsList.getFirstChild() as Identifier).getPayload()));
  } else {
    formalsList.setNextSibling(null);
    buffer.appendSibling(formalsList);
  }
  buffer.appendSibling(bodyStart);
  return buffer.toList(List);
}