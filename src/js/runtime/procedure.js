goog.provide('r5js.Procedure');


goog.require('r5js.Continuation');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.ProcedureLike');
goog.require('r5js.SiblingBuffer');
// TODO bl circular dependency goog.require('r5js.newProcCall');
goog.require('r5js.ast.CompoundDatum');
goog.require('r5js.ast.Identifier');
// TODO bl circular dependency goog.require('r5js.Environment');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Quote');
goog.require('r5js.datumutil');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');



/**
 * @param {!Array.<string>} formalsArray The procedure's formal parameters,
 *        in order.
 * @param {boolean} isDotted True iff this is a dotted procedure, such as
 *        (define (head x . y) x).
 * @param {?} bodyStart
 * @param {!r5js.IEnvironment} env An environment.
 * @param {string=} opt_name The procedure's name. It has no semantic
 *     importance; it's just used for pretty-printing debugs and messages
 *     to the user. If not given, one will be created.
 *     @implements {r5js.ProcedureLike}
 * @struct
 * @constructor
 */
r5js.Procedure = function(formalsArray, isDotted, bodyStart, env, opt_name) {

  /** @const @private */ this.formalsArray_ = formalsArray;
  /** @const @private */ this.isDotted_ = isDotted;
  /** @const @private */ this.name_ =
      goog.isDef(opt_name) ? opt_name : ('' + goog.getUid(this));
  /** @const @private {!r5js.IEnvironment} */ this.env_ =
      new r5js.Environment(env);

  if (bodyStart) {
    var helper = new r5js.Procedure.LetrecBindingsHelper_();
    var letrecBindings = helper.collectLetrecBindings(bodyStart);

    if (letrecBindings.isEmpty()) {
      this.body = helper.getLast().sequence(this.env_);
    } else {
      var letrec = new r5js.ast.List(letrecBindings.toSiblings());
      letrec.setNextSibling(/** @type {!r5js.Datum} */ (helper.getLast()));
      this.body = r5js.newProcCall(
          new r5js.ast.Identifier('letrec'),
          letrec,
          new r5js.Continuation());
    }
    this.lastContinuable = this.body.getLastContinuable();
  }
};
r5js.ProcedureLike.addImplementation(r5js.Procedure);


/**
 * @param {!r5js.Environment} env Environment to clone with.
 * @return {!r5js.Procedure} A clone of this procedure, with the given
 *         environment.
 */
r5js.Procedure.prototype.cloneWithEnv = function(env) {
  var ans = new r5js.Procedure(this.formalsArray_, this.isDotted_, null, env);
  ans.env_.setClosuresFrom(this.env_); // non-cloning ok?
  ans.body = this.body;
  ans.lastContinuable = this.lastContinuable;
  return ans;
};


/**
 * @param {!r5js.Continuation} c A continuation.
 * @private
 */
r5js.Procedure.prototype.setContinuation_ = function(c) {
  /* This will be a vacuous write for a tail call. But that is
     probably still faster than checking if we are in tail position and,
     if so, explicitly doing nothing. */
  if (this.lastContinuable) {
    this.lastContinuable.continuation = c;
  }
};


/**
 * @param {!r5js.Continuation} c A continuation.
 * @return {boolean} True iff this procedure is in tail position.
 * @private
 * TODO bl are we sure this covers all forms of tail recursion in R5RS?
 */
r5js.Procedure.prototype.isTailCall_ = function(c) {
  if (this.lastContinuable && this.lastContinuable.continuation === c) {
    // a good place to see if tail recursion is actually working :)
    // console.log('TAIL RECURSION!!!');
    return true;
  } else return false;
};


/** @override */
r5js.Procedure.prototype.toString = function() {
  return 'proc:' + this.name_;
};


/**
 * @param {!r5js.IEnvironment} env The environment to set.
 * @private
 */
r5js.Procedure.prototype.setEnv_ = function(env) {
  /* todo bl is it possible to have a procedure body whose first
     continuable is a branch? hopefully not, and I can remove
     the second check. */
  if (this.body) {
    //        if (this.body.subtype instanceof r5js.ProcCall) {
    if (this.body.subtype.setEnv) {
      this.body.subtype.setEnv(env, true);
    } else {
      throw new r5js.InternalInterpreterError(
          'invariant incorrect -- procedure does not begin with proc call');
    }
  }
};


/**
 * @param {number} numActuals The number of arguments passed to the procedure
 * during evaluation.
 * @private
 */
r5js.Procedure.prototype.checkNumArgs_ = function(numActuals) {

  if (!this.isDotted_) {
    if (numActuals !== this.formalsArray_.length)
      throw new r5js.IncorrectNumArgs(
          this.toString(), this.formalsArray_.length, numActuals);
  } else {
    var minNumArgs = this.formalsArray_.length - 1;
    if (numActuals < minNumArgs)
      throw new r5js.TooFewArgs(this.toString(), minNumArgs, numActuals);
  }
};


/**
 * @param {!Array.<!r5js.Datum>} args
 * @param {!r5js.IEnvironment} env
 * @private
 */
r5js.Procedure.prototype.bindArgs_ = function(args, env) {

  var name, i;

  for (i = 0; i < this.formalsArray_.length - 1; ++i) {
    name = this.formalsArray_[i];
    env.addBinding(name, args[i]);
  }

  if (this.formalsArray_.length > 0) {

    name = this.formalsArray_[i];
    if (!this.isDotted_) {
      env.addBinding(name, args[i]);
    } else {
      // Roll up the remaining arguments into a list
      var siblingBuffer = new r5js.SiblingBuffer();
      for (var j = this.formalsArray_.length - 1; j < args.length; ++j) {
        siblingBuffer.appendSibling(/** @type {!r5js.Datum} */ (args[j]));
      }
      env.addBinding(name, siblingBuffer.toList(r5js.ast.List));
    }
  }
};


/**
 * Non-primitive procedure, represented by {@link r5js.Procedure} object.
 * Example: suppose we have
 *
 * (define (foo x y) (+ x (* 2 y)))
 *
 * The body of this procedure is desugared as
 *
 * (* 2 y [_0 (+ x _0 [_1 ...])])
 *
 * Then we have the (nested) procedure call
 *
 * (+ 1 (foo 3 4))
 *
 * which is desugared as
 *
 * (foo 3 4 [foo' (+ 1 foo' [_2 ...])])
 *
 * We bind the arguments ("1" and "2") to the formal parameters ("x" and "y"),
 * append the ProcCall's continuation to the end of the Procedure's
 * continuation, and advance to the beginning of the Procedure's body.
 * Thus, on the next iteration of the trampoline loop, we will have
 * the following:
 *
 * (* 2 y [_0 (+ x _0 [foo' (+ 1 foo' [_2 ...])])])
 * @override
 * @suppress {accessControls} for procCall.env
 */
r5js.Procedure.prototype.evalAndAdvance = function(
    procCall, continuation, trampolineHelper, parserProvider) {

  /* If the operands aren't simple, we'll have to take a detour to
     restructure them. */
  if (!procCall.operandsInCpsStyle()) {
    procCall.cpsify(continuation, trampolineHelper, parserProvider);
  }

  else {

    // todo bl we should be able to pass false as the last parameter.
    // need to resolve some bugs.
    var args = procCall.evalArgs(true);

    /* If we're at a tail call we can reuse the existing environment.
         Otherwise create a new environment pointing back to the current one. */
    var newEnv = this.isTailCall_(continuation) ?
            procCall.env.allowRedefs() :
            new r5js.Environment(this.env_).addClosuresFrom(this.env_);

    /* Remember to discard the new environment
         at the end of the procedure call. */
    if (procCall.env) {
      continuation.rememberEnv(procCall.env);
    }

    // Do some bookkeepping to prepare for jumping into the procedure
    this.setContinuation_(continuation);
    this.checkNumArgs_(args.length);
    this.bindArgs_(args, newEnv);
    this.setEnv_(newEnv);

    // And away we go
    trampolineHelper.nextContinuable = this.body;
  }
};



/**
 * @struct
 * @constructor
 * @private
 */
r5js.Procedure.LetrecBindingsHelper_ = function() {
  /** @const @private */ this.bindings_ = new r5js.SiblingBuffer();
  /** @private {r5js.Datum} */ this.last_ = null;
};


/**
 * R5RS 5.2.2: "A <body> containing internal definitions can always be
 * converted into a completely equivalent letrec expression."
 * @param {!r5js.Datum} bodyStart
 * @return {!r5js.SiblingBuffer}
 */
r5js.Procedure.LetrecBindingsHelper_.prototype.collectLetrecBindings = function(
    bodyStart) {
  for (var cur = bodyStart;
      cur && cur.peekParse() === r5js.parse.Nonterminals.DEFINITION;
      cur = cur.getNextSibling()) {
    cur = /** @type {!r5js.ast.CompoundDatum} */ (cur);
    var firstChild = cur.getFirstChild();
    if (firstChild instanceof r5js.ast.Identifier &&
            firstChild.getPayload() === r5js.parse.Terminals.DEFINE) {
      this.bindings_.appendSibling(r5js.datumutil.extractDefinition(cur));
    } else {
      cur.forEachChild(this.collectLetrecBindingsForChild_, this);
    }
  }
  this.last_ = cur;
  return this.bindings_;
};


/**
 * @param {!r5js.Datum} node
 * @private
 */
r5js.Procedure.LetrecBindingsHelper_.prototype.collectLetrecBindingsForChild_ =
    function(node) {
  if (!(node instanceof r5js.ast.CompoundDatum) ||
      node instanceof r5js.ast.Quote) {
    return;
  }

  var firstChild = node.getFirstChild();

  if (firstChild instanceof r5js.ast.Identifier &&
      firstChild.getPayload() === r5js.parse.Terminals.DEFINE) {
    this.bindings_.appendSibling(r5js.datumutil.extractDefinition(node));
  } else if (node instanceof r5js.ast.CompoundDatum) {
    node.forEachChild(this.collectLetrecBindingsForChild_, this);
  }
};


/** @return {r5js.Datum} */
r5js.Procedure.LetrecBindingsHelper_.prototype.getLast = function() {
  return this.last_;
};





