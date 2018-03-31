/**
 * Typedef for Scheme values that this implementation represents with
 * primitive JavaScript values.
 */
export type PrimitiveValue = boolean | number | string;

/**
 * Interface for Scheme values that this implementation represents with
 * JavaScript objects.
 */
export interface ObjectValue {}

/** Top-level typedef for all Scheme values. */
export type Value = PrimitiveValue | ObjectValue;