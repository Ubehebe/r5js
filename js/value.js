goog.module('r5js.Value');

/**
 * Typedef for Scheme values that this implementation represents with
 * primitive JavaScript values.
 * @typedef {boolean|number|string}
 */
let PrimitiveValue;

/**
 * Interface for Scheme values that this implementation represents with
 * JavaScript objects.
 * @interface
 */
class ObjectValue {}

/**
 * Top-level typedef for all Scheme values.
 * @typedef {!PrimitiveValue|!ObjectValue}
 */
let Value;

exports = {ObjectValue, PrimitiveValue, Value};