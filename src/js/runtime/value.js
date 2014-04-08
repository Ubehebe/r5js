goog.provide('r5js.runtime.ObjectValue');
goog.provide('r5js.runtime.PrimitiveValue');
goog.provide('r5js.runtime.Value');


/**
 * Typedef for Scheme values that this implementation represents with
 * primitive JavaScript values.
 * @typedef {boolean|number|string|null}
 * TODO bl remove null.
 */
r5js.runtime.PrimitiveValue;



/**
 * Interface for Scheme values that this implementation represents with
 * JavaScript objects.
 * @interface
 */
r5js.runtime.ObjectValue = function() {};


/**
 * Top-level typedef for all Scheme values.
 * @typedef {r5js.runtime.PrimitiveValue|!r5js.runtime.ObjectValue}
 */
r5js.runtime.Value;
