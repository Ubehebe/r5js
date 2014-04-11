goog.provide('r5js.newAssignment');


goog.require('r5js.Continuable');
goog.require('r5js.GeneralSyntaxError');
goog.require('r5js.Macro');
goog.require('r5js.ProcCall');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.Identifier');
goog.require('r5js.runtime.UNSPECIFIED_VALUE');



/**
 * @param {?} firstOperand
 * @extends {r5js.ProcCall}
 * @struct
 * @constructor
 */
r5js.Assignment = function(firstOperand) {
  goog.base(this, r5js.Assignment.NAME_, firstOperand);

  /** @type {boolean} */ this.isSyntaxAssignment = false;

  /** @type {boolean} */ this.isTopLevelAssignment = false;
};
goog.inherits(r5js.Assignment, r5js.ProcCall);


/** @const @private */
r5js.Assignment.NAME_ = new r5js.ast.Identifier('set!');


/** @override */
r5js.Assignment.prototype.evalAndAdvance = function(
    continuation, resultStruct, envBuffer, parserProvider) {

  /* If the procedure call has no attached environment, we use
     the environment left over from the previous action on the trampoline. */
  if (!this.env) {
    this.setEnv(/** @type {!r5js.IEnvironment} */ (envBuffer.getEnv()));
  }

  this.tryAssignment_(continuation, resultStruct);

  /* Save the environment we used in case the next action on the trampoline
     needs it (for example branches, which have no environment of their own). */
  envBuffer.setEnv(/** @type {!r5js.IEnvironment} */(this.env));

  // We shouldn't leave the environment pointer hanging around.
  this.clearEnv();

  return resultStruct;
};


/**
 * @param {!r5js.Continuation} continuation
 * @param {!r5js.TrampolineHelper} resultStruct
 * @private
 */
r5js.Assignment.prototype.tryAssignment_ = function(
    continuation, resultStruct) {
  var src = this.env.get(/** @type {string} */ (
      this.firstOperand.getNextSibling().getPayload()));
  /* In Scheme, macros can be bound to identifiers but they are not really
     first-class citizens; you cannot say

     (define x let)

     because the text "let" does not parse as an expression
     (at least if it has its normal binding). In this implementation, however,
     SchemeMacros are objects that go into and come out of Environments
     like any other kind of objects. All kinds of assignments -- top-level,
     internal, syntax, non-syntax -- go through this function, so we
     have to make sure we don't accidentally permit some illegal behavior.

     If we're trying to assign a SchemeMacro object but the isSyntaxAssignment
     flag on the ProcCall object hasn't been set, then the programmer is
     requesting this assignment and we ought to signal an error.

     The situation is complicated a bit because internally, we use let and
     letrec to implement let-syntax and letrec-syntax. In other words,
     we as the implementer do exactly what we forbid the programmer to do.
     We tell the difference between the two parties via the isLetOrLetrecSyntax
     flag on the SchemeMacro object, which only the implementation can set. */
  if (src instanceof r5js.Macro &&
      !src.isLetOrLetrecSyntax() &&
      !this.isSyntaxAssignment) {
    throw new r5js.GeneralSyntaxError(this);
  }
  this.env.mutate(/** @type {string} */ (
      this.firstOperand.getPayload()), src, this.isTopLevelAssignment);
  /* The return value of an assignment is unspecified,
     but this is not the same as no binding. */
  this.bindResult(continuation, r5js.runtime.UNSPECIFIED_VALUE);
  resultStruct.nextContinuable = continuation.nextContinuable;
};


/**
 * @param {string} dstName
 * @param {string} srcName
 * @param {!r5js.Continuation} continuation
 * @return {!r5js.Continuable}
 */
r5js.newAssignment = function(dstName, srcName, continuation) {
  var operands = new r5js.SiblingBuffer()
        .appendSibling(new r5js.ast.Identifier(dstName))
        .appendSibling(new r5js.ast.Identifier(srcName))
        .toSiblings();
  return new r5js.Continuable(new r5js.Assignment(operands), continuation);
};

