goog.module('r5js.valutil');

const Character = goog.require('r5js.ast.Character');
const Datum = goog.require('r5js.Datum');
const EOF = goog.require('r5js.runtime.EOF');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const Quote = goog.require('r5js.ast.Quote');
const Ref = goog.require('r5js.Ref');
const String = goog.require('r5js.ast.String');
const UNSPECIFIED_VALUE = goog.require('r5js.UNSPECIFIED_VALUE');
const UserDefinedProcedure = goog.require('r5js.UserDefinedProcedure');
const Vector = goog.require('r5js.ast.Vector');
const {DottedList, List} = goog.require('r5js.ast.List');
const {Terminals} = goog.require('r5js.parse.Terminals');
const {Value} = goog.require('r5js.Value');

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
          children.splice(children.length - 1, 0, Terminals.DOT);
        }
        return Terminals.LPAREN +
            children.join(' ') +
            Terminals.RPAREN;
      } else if (value instanceof Vector) {
        const childStrings = value.mapChildren(
            goog.partial(toString, includeSigils)).join(' ');
        return Terminals.LPAREN_VECTOR +
            childStrings +
            Terminals.RPAREN;
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
        return Terminals.TICK + toString(
            includeSigils,
            /** @type {!Value} */ (value.getFirstChild()));
      } else if (value instanceof UserDefinedProcedure) {
        return '<proc:' + value.getName() + '>';
      } else if (InputPort.isImplementedBy(value)) {
        return '<input-port>';
      } else if (OutputPort.isImplementedBy(value)) {
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


