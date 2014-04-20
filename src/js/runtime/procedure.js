goog.provide('r5js.Procedure');


goog.require('r5js.Continuation');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.ProcCallLike');
goog.require('r5js.ProcedureLike');
goog.require('r5js.SiblingBuffer');
// TODO bl circular dependency goog.require('r5js.ProcCall');
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
 * @param {?} bodyStart
 * @param {!r5js.IEnvironment} env An environment.
 * @param {string=} opt_name The procedure's name. It has no semantic
 *     importance; it's just used for pretty-printing debugs and messages
 *     to the user. If not given, one will be created.
 * @implements {r5js.ProcedureLike}
 * @struct
 * @constructor
 */
r5js.Procedure = function(formalsArray, bodyStart, env, opt_name) {

  /** @const @protected */ this.formalsArray = formalsArray;
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
      this.body = new r5js.ProcCall(new r5js.ast.Identifier('letrec'), letrec);
    }
    this.lastContinuable = r5js.ProcCallLike.getLast(
        /** @type {!r5js.ProcCallLike} */ (this.body));
  }
};
r5js.ProcedureLike.addImplementation(r5js.Procedure);


/**
 * @param {!r5js.Environment} env Environment to clone with.
 * @return {!r5js.Procedure} A clone of this procedure, with the given
 *         environment.
 */
r5js.Procedure.prototype.cloneWithEnv = function(env) {
  var ans = new this.constructor(this.formalsArray, null /* bodyStart */, env);
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
    this.lastContinuable.setContinuation(c);
  }
};


/**
 * @param {!r5js.Continuation} c A continuation.
 * @return {boolean} True iff this procedure is in tail position.
 * @private
 * TODO bl are we sure this covers all forms of tail recursion in R5RS?
 */
r5js.Procedure.prototype.isTailCall_ = function(c) {
  if (this.lastContinuable &&
      this.lastContinuable.getContinuation() === c) {
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
  if (this.body) {
    (/** @type {!r5js.ProcCall} */ (this.body).setEnv(env, true));
  }
};


/**
 * @param {number} numActuals The number of arguments passed to the procedure
 * during evaluation.
 * @protected
 */
r5js.Procedure.prototype.checkNumArgs = function(numActuals) {
  if (numActuals !== this.formalsArray.length) {
    throw new r5js.IncorrectNumArgs(
        this.toString(), this.formalsArray.length, numActuals);
  }
};


/**
 * @param {!Array.<!r5js.runtime.Value>} args
 * @param {!r5js.IEnvironment} env
 * @protected
 */
r5js.Procedure.prototype.bindArgs = function(args, env) {
  for (var i = 0; i < this.formalsArray.length; ++i) {
    env.addBinding(this.formalsArray[i], args[i]);
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
    this.checkNumArgs(args.length);
    this.bindArgs(args, newEnv);
    this.setEnv_(newEnv);

    // And away we go
    trampolineHelper.setNextProcCallLike(
        /** @type {!r5js.ProcCallLike} */ (this.body));
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





