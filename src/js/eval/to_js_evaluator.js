goog.provide('r5js.ToJsEvaluator');


goog.require('r5js.Datum');
goog.require('r5js.Ref');
goog.require('r5js.ast.List');
goog.require('r5js.ast.String');
goog.require('r5js.ast.Vector');



/**
 * @param {!r5js.Evaluator} evaluator
 * @implements {r5js.EvaluateToExternalRepresentation.<?>}
 * @struct
 * @constructor
 */
r5js.ToJsEvaluator = function(evaluator) {
  this.evaluator_ = evaluator;
};


/** @override */
r5js.ToJsEvaluator.prototype.evaluate = function(input) {
  return r5js.ToJsEvaluator.schemeToJsValue_(this.evaluator_.evaluate(input));
};


/**
 * Maps Scheme values to idiomatic JavaScript values:
 *
 * Scheme strings -> JS strings
 * Scheme numbers -> JS numbers
 * Scheme booleans -> JS booleans
 * Scheme symbols -> JS strings
 * Scheme characters -> JS strings
 * Scheme proper lists -> JS arrays
 * Scheme vectors -> JS arrays
 *
 * This is just intended as a convenience when using the Scheme interpreter
 * from its JavaScript API. The mapping is somewhat arbitrary;
 * the two languages' type systems don't fit exactly. It is also noninjective,
 * so it won't work in the JS -> Scheme direction.
 *
 * @param {!r5js.runtime.Value} value
 * @return {boolean|number|string|Array|undefined}
 * @private
 */
r5js.ToJsEvaluator.schemeToJsValue_ = function(value) {
  switch (typeof value) {
    case 'number':
    case 'boolean':
    case 'string':
      return value;
    case 'object':
      if (value instanceof r5js.Ref) {
        return r5js.ToJsEvaluator.schemeToJsValue_(value.deref());
      } else if (value instanceof r5js.ast.List ||
          value instanceof r5js.ast.Vector) {
        return value.mapChildren(r5js.ToJsEvaluator.schemeToJsValue_);
      } else if (value instanceof r5js.ast.String ||
          value instanceof r5js.ast.Character) {
        return value.getPayload();
      } else if (value instanceof r5js.Datum) {
        return value.unwrap();
      }
    default:
      return undefined;
  }
};
