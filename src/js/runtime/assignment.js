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

goog.provide('r5js.Assignment');
goog.provide('r5js.newAssignment');

goog.require('r5js.Macro');
goog.require('r5js.ProcCallLike');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.Identifier');
goog.require('r5js.Error');
goog.require('r5js.runtime.UNSPECIFIED_VALUE');



/**
 * @param {r5js.Datum} firstOperand
 * @extends {r5js.ProcCallLike}
 * @struct
 * @constructor
 */
r5js.Assignment = function(firstOperand) {
  r5js.Assignment.base(this, 'constructor');

  /** @const @private */ this.firstOperand_ = firstOperand;
};
goog.inherits(r5js.Assignment, r5js.ProcCallLike);


/**
 * @override
 * @suppress {checkTypes} TODO bl
 */
r5js.Assignment.prototype.evalAndAdvance = function(
    resultStruct, envBuffer, parserProvider) {
  const src = this.getEnv().get(/** @type {string} */ (
      this.firstOperand_.getNextSibling().getPayload()));
  this.checkForImproperSyntaxAssignment(src);
  this.mutateEnv(/** @type {string} */ (this.firstOperand_.getPayload()), src);
  // R5RS 4.1.6: the value of an assignment is unspecified.
  resultStruct.setValue(r5js.runtime.UNSPECIFIED_VALUE);
  this.bindResult(r5js.runtime.UNSPECIFIED_VALUE);
  const nextContinuable = this.getNext();
  if (nextContinuable) {
    resultStruct.setNext(nextContinuable);
  }
};


/**
 * In Scheme, macros can be bound to identifiers but they are not really
 * first-class citizens; you cannot say (define x let) because the text "let"
 * does not parse as an expression (at least if it has its normal binding).
 * In this implementation, however, macros are objects that go into and come
 * out of environments like any other kind of objects. All kinds of assignments
 * -- top-level, internal, syntax, non-syntax -- go through this method,
 * so we have to make sure we don't accidentally permit some illegal behavior.
 *
 * If we're trying to assign a {@link r5js.Macro} here, the programmer is
 * requesting this assignment and we ought to signal an error.
 *
 * @see {r5js.TopLevelSyntaxAssignment#checkForImproperSyntaxAssignment},
 * which bypasses this check.
 *
 * @param {!r5js.runtime.Value} val
 * @protected
 */
r5js.Assignment.prototype.checkForImproperSyntaxAssignment = function(val) {
  if (val instanceof r5js.Macro && !val.isLetOrLetrecSyntax()) {
    throw r5js.Error.internalInterpreterError('TODO bl');
  }
};


/**
 * @param {string} name
 * @param {!r5js.runtime.Value} val
 * @protected
 */
r5js.Assignment.prototype.mutateEnv = function(name, val) {
  this.getEnv().mutate(name, val, false /* isTopLevel */);
};




/**
 * @param {string} dstName
 * @param {string} srcName
 * @return {!r5js.ProcCallLike}
 */
r5js.newAssignment = function(dstName, srcName) {
  const operands = new r5js.SiblingBuffer()
        .appendSibling(new r5js.ast.Identifier(dstName))
        .appendSibling(new r5js.ast.Identifier(srcName))
        .toSiblings();
  return new r5js.Assignment(operands);
};








