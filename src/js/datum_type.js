goog.provide('r5js.DatumType');


/** @typedef {string} */
r5js.Type;



/** @enum {!r5js.Type} */
r5js.DatumType = {
  BOOLEAN: 'boolean',
  CHARACTER: 'character',
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
  QUASIQUOTE: '`',
  QUOTE: "'",
  REF: 'ref',
  STRING: 'string',
  UNQUOTE: ',',
  UNQUOTE_SPLICING: ',@',
  VECTOR: '#('
};
