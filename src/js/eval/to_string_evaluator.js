goog.provide('r5js.ToStringEvaluator');


goog.require('r5js.OutputMode');
goog.require('r5js.datumutil');



/**
 * @param {!r5js.Evaluator} evaluator
 * @implements {r5js.EvaluateToExternalRepresentation.<string>}
 * @struct
 * @constructor
 */
r5js.ToStringEvaluator = function(evaluator) {
  /** @const @private */ this.evaluator_ = evaluator;
};


/** @override */
r5js.ToStringEvaluator.prototype.evaluate = function(input) {
  return r5js.ToStringEvaluator.schemeValueToString(
      this.evaluator_.evaluate(input));
};


/**
 * @param {!r5js.runtime.Value} value
 * @return {string}
 */
r5js.ToStringEvaluator.schemeValueToString = function(value) {
  switch (typeof value) {
    case 'number':
      return value + '';
    case 'boolean':
      return value ? '#t' : '#f';
    case 'string':
      return '"' + value + '"'; // TODO bl escape " and \
    case 'object':
      if (value instanceof r5js.Ref) {
        return r5js.ToStringEvaluator.schemeValueToString(value.deref());
      } else if (value instanceof r5js.ast.List) {
        var childStrings =
            value.mapChildren(r5js.ToJsEvaluator.schemeToJsValue).join(' ');
        return '(' + childStrings + ')';
      } else if (value instanceof r5js.ast.Vector) {
        var childStrings =
            value.mapChildren(r5js.ToJsEvaluator.schemeToJsValue).join(' ');
        return '#(' + childStrings;
      } else if (value instanceof r5js.ast.String) {
        return '"' + value.getPayload() + '"'; // TODO bl escape
      } else if (value instanceof r5js.ast.Character) {
        return '#\\' + value.getPayload();
      } else if (value instanceof r5js.Datum) {
        return r5js.ToStringEvaluator.schemeValueToString(value.unwrap());
      }
    default:
      return '';
  }
};
