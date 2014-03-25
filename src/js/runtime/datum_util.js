goog.provide('r5js.datumutil');


// TODO bl circular dependency goog.require('r5js.Continuation');
goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.JsObjOrMethod');
goog.require('r5js.Macro');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.Boolean');
goog.require('r5js.ast.Character');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Node');
goog.require('r5js.ast.Number');
goog.require('r5js.ast.String');
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
 * @param {!r5js.PayloadType} result The result to potentially wrap.
 * @param {!r5js.Type=} opt_type TODO bl.
 * @return {r5js.PayloadType} The result, wrapped in a {@link r5js.Datum}
 *         if necessary.
 * TODO bl: remove. This whole method seems confused.
 */
r5js.datumutil.maybeWrapResult = function(result, opt_type) {

  if (result === null ||
      result instanceof r5js.Datum ||
      result instanceof r5js.Continuation ||
      result instanceof r5js.Macro ||
      result instanceof r5js.JsObjOrMethod /* JS interop (experimental) */ ||
      r5js.ast.Node.isImplementedBy(result)) {
    return result; // no-op, strictly for convenience
  }

  var ans = new r5js.Datum();
  ans.setPayload(result);
  if (opt_type === r5js.DatumType.BOOLEAN) {
    return new r5js.ast.Boolean(/** @type {boolean} */ (result));
  } else if (opt_type === r5js.DatumType.CHARACTER) {
    return new r5js.ast.Character(/** @type {string} */ (result));
  } else if (opt_type === r5js.DatumType.NUMBER) {
    return new r5js.ast.Number(/** @type {number} */ (result));
  } else if (opt_type === r5js.DatumType.STRING) {
    return new r5js.ast.String(/** @type {string} */ (result));
  } else if (goog.isDef(opt_type)) {
    ans.setType(opt_type);
  } else {
    // If no type was supplied, we can deduce it in most (not all) cases
    switch (typeof result) {
      case 'boolean':
        return new r5js.ast.Boolean(result);
      case 'number':
        return new r5js.ast.Number(result);
      case 'string':
        return new r5js.ast.Identifier(result);
      case 'object':
        if (result instanceof r5js.Procedure) {
          ans.setType(r5js.parse.Terminals.LAMBDA);
          break;
        }
      default:
        throw new r5js.InternalInterpreterError(
            'cannot deduce type from value ' +
                result +
                ': noninjective mapping from values to types');
    }
  }
  return ans;
};
