/* Copyright 2011-2014 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

goog.provide('r5js.valutil');



goog.require('r5js.Environment');
goog.require('r5js.InputPort');
goog.require('r5js.OutputPort');
goog.require('r5js.UserDefinedProcedure');
goog.require('r5js.ast.Quote');
goog.require('r5js.parse.Terminals');
goog.require('r5js.runtime.UNSPECIFIED_VALUE');

/*
 * Implementation note: a richer representation of Scheme values would
 * be useful for any embedded use of the interpreter. I tried a few times
 * to design a richer API, but it was never very good. There were two main
 * difficulties. First, because {@link r5js.Evaluator#evaluate} is asynchronous,
 * the interpreter should be able to run in a client-server fashion.
 * (For example, in {@link r5js.platform.Html5}, the actual interpreter
 * runs in a web worker, and communicates with a client stub via postMessage.)
 * This restricts the external representation of Scheme values.
 * Simply passing {@link r5js.runtime.Value} instances does not work,
 * because the HTML5 structured clone algorithm does not serialize functions
 * (and many {@link r5js.runtime.Value} implementations have rich method sets).
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
 * @param {!r5js.runtime.Value} value
 * @return {string}
 */
r5js.valutil.toDisplayString = function(value) {
  return r5js.valutil.toString_(false /* includeSigils */, value);
};


/**
 * @param {!r5js.runtime.Value} value
 * @return {string}
 */
r5js.valutil.toWriteString = function(value) {
  return r5js.valutil.toString_(true /* includeSigils */, value);
};


/**
 * @param {boolean} includeSigils
 * @param {!r5js.runtime.Value} value
 * @return {string}
 * @private
 */
r5js.valutil.toString_ = function(includeSigils, value) {
  switch (typeof value) {
    case 'number':
      return value + '';
    case 'boolean':
      return value ? '#t' : '#f';
    case 'string':
      return value;
    case 'object':
      if (value === r5js.runtime.UNSPECIFIED_VALUE) {
        return '';
      } else if (value === r5js.runtime.EOF) {
        return '<eof>';
      } else if (value instanceof r5js.Ref) {
        return r5js.valutil.toString_(includeSigils, value.deref());
      } else if (value instanceof r5js.ast.List ||
          value instanceof r5js.ast.DottedList) {
        var children = value.mapChildren(
            goog.partial(r5js.valutil.toString_, includeSigils));
        if ((value instanceof r5js.ast.List && value.isImproperList()) ||
            value instanceof r5js.ast.DottedList) {
          children.splice(children.length - 1, 0, r5js.parse.Terminals.DOT);
        }
        return r5js.parse.Terminals.LPAREN +
            children.join(' ') +
            r5js.parse.Terminals.RPAREN;
      } else if (value instanceof r5js.ast.Vector) {
        var childStrings = value.mapChildren(
            goog.partial(r5js.valutil.toString_, includeSigils)).join(' ');
        return r5js.parse.Terminals.LPAREN_VECTOR +
            childStrings +
            r5js.parse.Terminals.RPAREN;
      } else if (value instanceof r5js.ast.String) {
        return includeSigils ?
            '"' + value.getPayload() + '"' : // TODO bl escape
            value.getPayload();
      } else if (value instanceof r5js.ast.Character) {
        if (includeSigils) {
          // Special cases for space and newline: R5RS 6.3.4
          var payload = value.getPayload();
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
      } else if (value instanceof r5js.ast.Quote) {
        return r5js.parse.Terminals.TICK + r5js.valutil.toString_(
            includeSigils,
            /** @type {!r5js.runtime.Value} */ (value.getFirstChild()));
      } else if (value instanceof r5js.UserDefinedProcedure) {
        return '<proc:' + value.getName() + '>';
      } else if (value instanceof r5js.procspec.PrimitiveProcedure_) {
        return '<proc:' + value.getDebugName() + '>';
      } else if (r5js.InputPort.isImplementedBy(value)) {
        return '<input-port>';
      } else if (r5js.OutputPort.isImplementedBy(value)) {
        return '<output-port>';
      } else if (value instanceof r5js.Datum) {
        return r5js.valutil.toString_(includeSigils, value.unwrap());
      } else if (value instanceof r5js.Environment) {
        return '<environment-specifier>';
      }
    default:
      return '';
  }
};


