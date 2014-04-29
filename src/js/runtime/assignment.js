goog.provide('r5js.newAssignment');
goog.provide('r5js.newTopLevelAssignment');
goog.provide('r5js.newTopLevelSyntaxAssignment');


goog.require('r5js.GeneralSyntaxError');
goog.require('r5js.Macro');
goog.require('r5js.ProcCallLike');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.Identifier');
goog.require('r5js.runtime.UNSPECIFIED_VALUE');



/**
 * @param {r5js.Datum} firstOperand
 * @extends {r5js.ProcCallLike}
 * @struct
 * @constructor
 */
r5js.Assignment = function(firstOperand) {
  goog.base(this);

  /** @const @private */ this.firstOperand_ = firstOperand;
};
goog.inherits(r5js.Assignment, r5js.ProcCallLike);


/**
 * @override
 * @suppress {checkTypes} TODO bl
 */
r5js.Assignment.prototype.evalAndAdvance = function(
    resultStruct, envBuffer, parserProvider) {
  var src = this.getEnv().get(/** @type {string} */ (
      this.firstOperand_.getNextSibling().getPayload()));
  this.checkForImproperSyntaxAssignment(src);
  this.mutateEnv(/** @type {string} */ (this.firstOperand_.getPayload()), src);
  /* The return value of an assignment is unspecified,
     but this is not the same as no binding. */
  this.bindResult(r5js.runtime.UNSPECIFIED_VALUE);
  var nextContinuable = this.getNext();
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
    throw new r5js.GeneralSyntaxError(this);
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
 * @param {r5js.Datum} firstOperand
 * @extends {r5js.Assignment}
 * @struct
 * @constructor
 */
r5js.TopLevelAssignment = function(firstOperand) {
  goog.base(this, firstOperand);
};
goog.inherits(r5js.TopLevelAssignment, r5js.Assignment);


/** @override */
r5js.TopLevelAssignment.prototype.mutateEnv = function(name, val) {
  this.getEnv().mutate(name, val, true /* isTopLevel */);
};



/**
 * @param {r5js.Datum} firstOperand
 * @extends {r5js.TopLevelAssignment}
 * @struct
 * @constructor
 */
r5js.TopLevelSyntaxAssignment = function(firstOperand) {
  goog.base(this, firstOperand);
};
goog.inherits(r5js.TopLevelSyntaxAssignment, r5js.TopLevelAssignment);


/** @override */
r5js.TopLevelSyntaxAssignment.prototype.checkForImproperSyntaxAssignment =
    goog.nullFunction;


/**
 * @param {string} dstName
 * @param {string} srcName
 * @return {!r5js.ProcCallLike}
 */
r5js.newAssignment = function(dstName, srcName) {
  var operands = new r5js.SiblingBuffer()
        .appendSibling(new r5js.ast.Identifier(dstName))
        .appendSibling(new r5js.ast.Identifier(srcName))
        .toSiblings();
  return new r5js.Assignment(operands);
};


/**
 * @param {string} dstName
 * @param {string} srcName
 * @return {!r5js.ProcCallLike}
 */
r5js.newTopLevelAssignment = function(dstName, srcName) {
  var operands = new r5js.SiblingBuffer()
        .appendSibling(new r5js.ast.Identifier(dstName))
        .appendSibling(new r5js.ast.Identifier(srcName))
        .toSiblings();
  return new r5js.TopLevelAssignment(operands);
};


/**
 * @param {string} dstName
 * @param {string} srcName
 * @return {!r5js.ProcCallLike}
 */
r5js.newTopLevelSyntaxAssignment = function(dstName, srcName) {
  var operands = new r5js.SiblingBuffer()
        .appendSibling(new r5js.ast.Identifier(dstName))
        .appendSibling(new r5js.ast.Identifier(srcName))
        .toSiblings();
  return new r5js.TopLevelSyntaxAssignment(operands);
};









