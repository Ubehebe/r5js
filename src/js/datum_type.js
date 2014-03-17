goog.provide('r5js.DatumType');


/** @typedef {string} */
r5js.Type;


/**
 * @enum {!r5js.Type}
 * TODO bl: remove. Type enums are inferior to subclasses as a way
 * of structuring code. There is one major place where direct type checks are
 * probably unavoidable -- runtime argument type-checking -- but that can be
 * done directly with instanceof constructor checks, or opaque isXXX()
 * functions.
 */
r5js.DatumType = {
  BOOLEAN: 'boolean',
  CHARACTER: 'char',
  DOTTED_LIST: '.(',
  ENVIRONMENT_SPECIFIER: 'environment-specifier',
  FFI: 'ffi',
  IDENTIFIER: 'identifier',
  INPUT_PORT: 'input-port',
  LAMBDA: 'lambda',
  LIST: '(',
  MACRO: 'macro',
  NUMBER: 'number',
  OUTPUT_PORT: 'output-port',
  PAIR: 'pair', // TODO bl
  REF: 'ref',
  STRING: 'string',
  SYMBOL: 'symbol',
  VECTOR: '#('
};
