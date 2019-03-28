import {Type} from "./type";

/**
 * This is basically the enumeration of types in R5RS 3.2.
 * Ordinarily, type tags are a code smell in object-oriented code.
 * But this implementation is not completely object-oriented; in particular,
 * Scheme booleans, numbers, and symbols are represented by
 * corresponding JavaScript primitives, not objects.
 * It is convenient to have names for these types, for example
 * when printing errors during runtime type-checking. But for the most part
 * they have no critical role in this implementation.
 */
export const BOOLEAN =  new Type('boolean');
export const CHARACTER =  new Type('char');
export const ENVIRONMENT_SPECIFIER =  new Type('environment-specifier');
export const INPUT_PORT =  new Type('input-port');
export const NUMBER =  new Type('number');
export const OUTPUT_PORT =  new Type('output-port');
export const PAIR =  new Type('pair');
export const PROCEDURE =  new Type('procedure');
export const STRING =  new Type('string');
export const SYMBOL =  new Type('symbol');
export const VECTOR = new Type('vector');