goog.provide('r5js.Procedure');


goog.require('r5js.Continuation');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.SiblingBuffer');
// TODO bl circular dependency goog.require('r5js.procs');
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
 * @struct
 * @constructor
 */
r5js.Procedure = function(formalsArray, isDotted, bodyStart, env, opt_name) {

  /** @const @private {!Array.<string>} */
  this.formalsArray_ = formalsArray;

  /** @const @private */ this.isDotted_ = isDotted;

  /** @const @private */ this.name_ =
      goog.isDef(opt_name) ? opt_name : ('' + goog.getUid(this));

  /** @const @private {!r5js.IEnvironment} */
  this.env_ = new r5js.Environment(env);

  if (bodyStart) {
    var helper = new r5js.Procedure.LetrecBindingsHelper_();
    var letrecBindings = helper.collectLetrecBindings(bodyStart);

    if (letrecBindings.isEmpty()) {
      this.body = helper.getLast().sequence(this.env_);
    } else {
      var letrec = new r5js.ast.List(letrecBindings.toSiblings());
      letrec.setNextSibling(/** @type {!r5js.Datum} */ (helper.getLast()));
      this.body = r5js.procs.newProcCall(
          new r5js.ast.Identifier('letrec'),
          letrec,
          new r5js.Continuation());
    }
    this.lastContinuable = this.body.getLastContinuable();
  }
};


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
 */
r5js.Procedure.prototype.setContinuation = function(c) {
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
 * TODO bl are we sure this covers all forms of tail recursion in R5RS?
 */
r5js.Procedure.prototype.isTailCall = function(c) {
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
 */
r5js.Procedure.prototype.setEnv = function(env) {
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
 *        during evaluation.
 */
r5js.Procedure.prototype.checkNumArgs = function(numActuals) {

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
 */
r5js.Procedure.prototype.bindArgs = function(args, env) {

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





