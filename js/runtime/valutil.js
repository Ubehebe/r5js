goog.module('r5js.valutil');

const {Character} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/character');
const {Datum, UNSPECIFIED_VALUE} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {EOF} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/eof');
const {isInputPortImpl} = require('/js/io/io_collect_es6_sources.es6/node_modules/__main__/js/io/input_port');
const {Quote} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/quote');
const {Ref} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/ref');
const {String} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/string');
const {UserDefinedProcedure} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/user_defined_procedure');
const {Vector} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/vector');
const {DottedList, List} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/list');
const {DOT, LPAREN, LPAREN_VECTOR, RPAREN, TICK} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/parse/terminals');
const {isOutputPortImpl} = require('/js/io/io_collect_es6_sources.es6/node_modules/__main__/js/io/output_port');

/*
 * Implementation note: a richer representation of Scheme values would
 * be useful for any embedded use of the interpreter. I tried a few times
 * to design a richer API, but it was never very good. There were two main
 * difficulties. First, because {@link r5js.Evaluator#evaluate} is asynchronous,
 * the interpreter should be able to run in a client-server fashion.
 * (For example, in {@link r5js.platform.Html5_}, the actual interpreter
 * runs in a web worker, and communicates with a client stub via postMessage.)
 * This restricts the external representation of Scheme values.
 * Simply passing Value instances does not work, because the HTML5 structured
 * clone algorithm does not serialize functions (and many Value implementations
 * have rich method sets).
 *
 * The second and more fundamental problem is that the Scheme and JavaScript
 * type systems don't align very well beyond booleans, numbers, and strings.
 * JavaScript doesn't distinguish between lists and vectors or among
 * characters, strings, and symbols. For other Scheme values, there is
 * no reasonable representation in JavaScript: ports, procedures, environment
 * specifiers. (The evaluator cannot simply hand over its internal
 * reprsentations of these due to the first problem above.) So I ended up
 * mapping these to the JavaScript undefined value, and this didn't feel
 * satisfactory.
 */

/**
 * @param {!Value} value
 * @return {string}
 */
function toDisplayString(value) {
  return toString(false /* includeSigils */, value);
}

/**
 * @param {!Value} value
 * @return {string}
 */
function toWriteString(value) {
  return toString(true /* includeSigils */, value);
}

/**
 * @param {boolean} includeSigils
 * @param {!Value} value
 * @return {string}
 */
function toString(includeSigils, value) {
  switch (typeof value) {
    case 'number':
      return value + '';
    case 'boolean':
      return value ? '#t' : '#f';
    case 'string':
      return value;
    case 'object':
      if (value === UNSPECIFIED_VALUE) {
        return '';
      } else if (value === EOF) {
        return '<eof>';
      } else if (value instanceof Ref) {
        return toString(includeSigils, value.deref());
      } else if (value instanceof List || value instanceof DottedList) {
        const children = value.mapChildren(
            goog.partial(toString, includeSigils));
        if ((value instanceof List && value.isImproperList())
            || value instanceof DottedList) {
          children.splice(children.length - 1, 0, DOT);
        }
        return LPAREN +
            children.join(' ') +
            RPAREN;
      } else if (value instanceof Vector) {
        const childStrings = value.mapChildren(
            goog.partial(toString, includeSigils)).join(' ');
        return LPAREN_VECTOR +
            childStrings +
            RPAREN;
      } else if (value instanceof String) {
        return includeSigils ?
            '"' + value.getPayload() + '"' : // TODO bl escape
            value.getPayload();
      } else if (value instanceof Character) {
        if (includeSigils) {
          // Special cases for space and newline: R5RS 6.3.4
          const payload = value.getPayload();
          if (payload === ' ') {
            return '#\\space';
          } else if (payload === '\n') {
            return '#\\newline';
          } else {
            return '#\\' + payload;
          }
        } else {
          return value.getPayload();
        }
      } else if (value instanceof Quote) {
        return TICK + toString(
            includeSigils,
            /** @type {!Value} */ (value.getFirstChild()));
      } else if (value instanceof UserDefinedProcedure) {
        return '<proc:' + value.getName() + '>';
      } else if (isInputPortImpl(value)) {
        return '<input-port>';
      } else if (isOutputPortImpl(value)) {
        return '<output-port>';
      } else if (value instanceof Datum) {
        return toString(includeSigils, value.unwrap());
      } else {
        return value.toString();
      }
    default:
      return '';
  }
}

exports = {
    toDisplayString: toDisplayString,
    toWriteString: toWriteString
};


