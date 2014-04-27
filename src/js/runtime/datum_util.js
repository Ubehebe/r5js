goog.provide('r5js.datumutil');


goog.require('r5js.AbstractProcedure');
goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.IEnvironment');
goog.require('r5js.InputPort');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.Macro');
goog.require('r5js.OutputPort');
goog.require('r5js.ProcedureLike');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.Boolean');
goog.require('r5js.ast.Character');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.Lambda');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Number');
goog.require('r5js.ast.String');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.runtime.UNSPECIFIED_VALUE');


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
 * @param {?} result The result to potentially wrap.
 * @param {!r5js.Type=} opt_type
 * @return {?} The result, wrapped in a {@link r5js.Datum} if necessary.
 * @suppress {accessControls} for result.name_.
 * TODO bl: remove. This whole method seems confused.
 */
r5js.datumutil.maybeWrapResult = function(result, opt_type) {

  if (result === null) {
    // TODO bl don't allow passing in null
    return r5js.runtime.UNSPECIFIED_VALUE;
  } else if (result === r5js.runtime.UNSPECIFIED_VALUE ||
      result instanceof r5js.Datum ||
      result instanceof r5js.Macro ||
      result instanceof r5js.AbstractProcedure ||
      r5js.ProcedureLike.isImplementedBy(result) ||
      r5js.IEnvironment.isImplementedBy(result) ||
      r5js.InputPort.isImplementedBy(result) ||
      r5js.OutputPort.isImplementedBy(result)) {
    return result; // no-op, strictly for convenience
  } else if (opt_type === r5js.DatumType.CHARACTER) {
    return new r5js.ast.Character(/** @type {string} */ (result));
  } else if (opt_type === r5js.DatumType.NUMBER) {
    return new r5js.ast.Number(/** @type {number} */ (result));
  } else if (opt_type === r5js.DatumType.STRING) {
    return new r5js.ast.String(/** @type {string} */ (result));
  } else {
    // If no type was supplied, we can deduce it in most (not all) cases
    switch (typeof result) {
      case 'boolean':
        return new r5js.ast.Boolean(result);
      case 'number':
        return new r5js.ast.Number(result);
      case 'string':
        return new r5js.ast.Identifier(result);
      default:
        throw new r5js.InternalInterpreterError(
            'cannot deduce type from value ' +
                result +
                ': noninjective mapping from values to types');
    }
  }
};
