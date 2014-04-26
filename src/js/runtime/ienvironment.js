/* Copyright 2011-2013 Brendan Linn

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


goog.provide('r5js.IEnvironment');



/**
 * Interface abstracted from {@link r5js.Environment}.
 *
 * An Environment stores three common kinds of objects:
 * - Datums (most Scheme values: numbers, identifiers, etc.)
 * - SchemeProcedures (native Scheme procedures)
 * - JavaScript functions ('primitive' Scheme procedures)
 *
 * There is a fourth kind of object, a Continuation, which can get stored
 * when calling "magical" procedures like call/cc, where the current
 * continuation is bound to a formal parameter.
 *
 * {@link r5js.IEnvironment.get} will only ever return Datums and Continuations;
 * it will wrap SchemeProcedures and JavaScript functions in Datum
 * wrappers before returning, to allow for things like
 *
 * (cons + (lambda () "hi!"))
 *
 * A drawback is that comparisons on stuff retrieved from an Environment
 * may need to be unwrapped:
 *
 * var x = env.get('foo');
 * var y = env.get('foo');
 * x == y // false if foo is a SchemeProcedure or JavaScript function!
 *
 * If you know your key should retrieve a SchemeProcedure or JavaScript
 * function, you can use {@link r5js.IEnvironment.getProcedure} to avoid the
 * wrapping and unwrapping.
 *
 * @extends {r5js.runtime.ObjectValue}
 * @interface
 */
r5js.IEnvironment = function() {};


/**
 * @param {string} name Name of the binding.
 * @param {!r5js.runtime.Value} val Value of the binding.
 */
r5js.IEnvironment.prototype.addBinding = function(name, val) {};


/**
 * Used exclusively during desugaring of lambda expressions.
 *
 * Lambda expressions have much in common with procedure definitions,
 * even though they don't introduce a new binding (in a programmer-visible
 * way, at least). For example:
 *
 * (define (foo x) (define (bar y) (+ x y)) bar)
 *
 * (define (foo x) (lambda (y) (+ x y)))
 *
 * With either definition of foo, we must have
 *
 * ((foo 10) 11) => 22
 *
 * With internal definitions, this is easy. The grammar of Scheme says that
 * all internal definitions must precede all expressions in a procedure body,
 * so the {@link r5js.Procedure} constructor can intercept all the definitions
 * and deal with them appropriately.
 *
 * Lambda expressions, however, can appear anywhere in a procedure's body,
 * so we deal with them in a generic way here. Using the second definition of
 * foo above as an example, here's what happens:
 *
 * - During parsing of foo, we create a new {@link r5js.IEnvironment}
 *   for the procedure (say, fooEnv), and note all foo's lambdas in fooEnv,
 *   using {@link r5js.IEnvironment.addClosure}.
 * - Later, when we want to evaluate (foo 10), we create a new
 *   {@link r5js.IEnvironment} hanging off fooEnv (say, tmp-fooEnv).
 *   (We have to do this to support multiple active calls to the same
 *   procedure.) We copy all of fooEnv's closures into tmp-fooEnv as actual
 *   bound {@link r5js.Procedure}s, using
 *   {@link r5js.Environment.addClosuresFrom}.
 *   We also bind the arguments (in this case x = 10) in tmp-fooEnv,
 *   then advance to foo's body.
 *
 * In this way, when we get to the body of the lambda expression, both x and y
 * are already in scope. The key point is that the environment
 * of (lambda (y) (+ x y)) points back to the environment representing the
 * _execution_ of foo (tmp-fooEnv), not the Environment representing foo itself
 * (fooEnv).
 *
 * @param {string} name Name of the binding to install this closure under.
 * @param {!r5js.Procedure} proc Closure to install.
 * TODO bl: consider renaming to addSchemeProcedure?
 */
r5js.IEnvironment.prototype.addClosure = function(name, proc) {};


/**
 * @param {string} name Name of binding to get.
 * @return {!r5js.runtime.Value|null} Value of binding, if any.
 */
r5js.IEnvironment.prototype.get = function(name) {};


/**
 * @param {string} name Name of the procedure to get.
 * @return {!r5js.runtime.Value|null} Value of binding, if any.
 */
r5js.IEnvironment.prototype.getProcedure = function(name) {};


/**
 * @param {string} name Name of the binding to look up.
 * @return {boolean} True iff the environment, or any of its enclosing
 * environments, has a binding for the name.
 */
r5js.IEnvironment.prototype.hasBindingRecursive = function(name) {};


/**
 * R5RS 5.2.1: "At the top level of a program, a definition
 *
 * (define <variable> <expression>)
 *
 * has essentially the same effect as the assignment expression
 *
 * (set! <variable> <expression>)
 *
 * if <variable> is bound. If <variable> is not bound, however, then
 * the definition will bind <variable> to a new location before performing
 * the assignment, whereas it would be an error to perform a set! on
 * an unbound variable."
 *
 * We use the isTopLevel parameter to perform the override mentioned.
 *
 * @param {string} name Name of the binding.
 * @param {!r5js.runtime.Value} newVal New value of the binding.
 * @param {boolean} isTopLevel True iff the binding should be top-level.
 */
r5js.IEnvironment.prototype.mutate = function(name, newVal, isTopLevel) {};


/**
 * Just for environments defined in the standard; users shouldn't be able to
 * add to them.
 */
r5js.IEnvironment.prototype.seal = function() {};


/** @const @private */
r5js.IEnvironment.IMPLEMENTED_BY_PROP_ = '$r5js.IEnvironment';


/**
 * @param {*} obj
 * @return {boolean}
 * TODO bl temporary shim. Remove.
 */
r5js.IEnvironment.isImplementedBy = function(obj) {
  return !!(obj && obj[r5js.IEnvironment.IMPLEMENTED_BY_PROP_]);
};


/** @param {function(new: r5js.IEnvironment, ...)} ctor */
r5js.IEnvironment.addImplementation = function(ctor) {
  ctor.prototype[r5js.IEnvironment.IMPLEMENTED_BY_PROP_] = true;
};

