// The declarations in this file were originally Closure Compiler @typedefs.
// I first tried to translate them to TypeScript `export type` declarations.
// tsickle emitted something like `/** @typedef {...} */ exports.PrimitiveValue;`.
// That's appropriate for goog.modules, but causes the Closure Compiler parser to crash
// when encountered in an ES6 module (presumably because of the `export` keyword).
// Fortunately, since this file is just used for type-checking, we can emit TypeScript `declare`
// statements instead.
// TODO: maybe rename to d.ts?

/**
 * Typedef for Scheme values that this implementation represents with
 * primitive JavaScript values.
 */
declare type PrimitiveValue = boolean | number | string;

/**
 * Interface for Scheme values that this implementation represents with
 * JavaScript objects.
 */
declare interface ObjectValue {}

/** Top-level typedef for all Scheme values. */
declare type Value = PrimitiveValue | ObjectValue;