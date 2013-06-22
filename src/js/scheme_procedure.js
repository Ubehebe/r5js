/* Copyright 2011, 2012 Brendan Linn

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


goog.provide('r5js.Procedure');


goog.require('r5js.Continuation');
goog.require('r5js.data');
goog.require('r5js.IncorrectNumArgs');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.TooFewArgs');

/**
 * @param {!Array.<string>} formalsArray The procedure's formal parameters,
 *        in order.
 * @param {boolean} isDotted True iff this is a dotted procedure, such as
 *        (define (head x . y) x).
 * @param {?} bodyStart
 * @param {!r5js.IEnvironment} env An environment.
 * @param {string} name The procedure's name. It has no semantic importance;
 *        it's just used for pretty-printing debugs and messages to the user.
 * @constructor
 */
r5js.Procedure = function(formalsArray, isDotted, bodyStart, env, name) {
    /**
     * @type {boolean}
     */
    this.isDotted = isDotted;

    /**
     * @type {!r5js.IEnvironment}
     */
    this.env = env.newChildEnv(name);

    /**
     * @type {!Array.<string>}
     */
    this.formalsArray = formalsArray;

    /**
     * @type {string}
     */
    this.name = name;

    if (bodyStart) {

        /* R5RS 5.2.2: "A <body> containing internal definitions can always
        be converted into a completely equivalent letrec expression." */
        var letrecBindings = new r5js.SiblingBuffer();
        for (var cur = bodyStart; cur && cur.peekParse() === 'definition'; cur = cur.nextSibling) {
                cur.forEach(function(node) {
                    if (node.firstChild && node.firstChild.payload === 'define')
                        letrecBindings.appendSibling(node.extractDefinition());
                });
        }

        if (letrecBindings.isEmpty()) {
            this.body = cur.sequence(this.env);
        } else {
            var letrec = newEmptyList();
            letrec.firstChild = letrecBindings.toSiblings();
            letrec.nextSibling = cur;
            this.body = r5js.data.newProcCall(
                r5js.data.newIdOrLiteral('letrec'),
                letrec,
                new r5js.Continuation(newCpsName())
            );
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
    var ans = new r5js.Procedure(this.formalsArray, this.isDotted, null, env, this.name + "'-" + (uniqueNodeCounter++));
    ans.env.setClosuresFrom(this.env); // non-cloning ok?
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
    return 'proc:' + this.name;
};


/**
 * @param {number} numActuals The number of arguments passed to the procedure
 *        during evaluation.
 */
r5js.Procedure.prototype.checkNumArgs = function(numActuals) {

    if (!this.isDotted) {
        if (numActuals !== this.formalsArray.length)
            throw new r5js.IncorrectNumArgs(this.toString(), this.formalsArray.length, numActuals);
    } else {
        var minNumArgs = this.formalsArray.length - 1;
        if (numActuals < minNumArgs)
            throw new r5js.TooFewArgs(this.toString(), minNumArgs, numActuals);
    }
};


/**
 * @param args
 * @param {!r5js.IEnvironment} env
 */
r5js.Procedure.prototype.bindArgs = function(args, env) {

    var name, i;

    for (i = 0; i < this.formalsArray.length - 1; ++i) {
        name = this.formalsArray[i];
        env.addBinding(name, args[i]);
    }

    if (this.formalsArray.length > 0) {

        name = this.formalsArray[i];
        if (!this.isDotted) {
            env.addBinding(name, args[i]);
        } else {
            // Roll up the remaining arguments into a list
            var list = newEmptyList();
            // Go backwards and do prepends to avoid quadratic performance
            for (var j = args.length - 1; j >= this.formalsArray.length - 1; --j)
                list.prependChild(args[j]);
            env.addBinding(name, list);
        }
    }
};