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


goog.provide('r5js.Environment');


goog.require('r5js.EvalError');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.UnboundVariable');


/**
 * @param {string} name The environment's name. Just for pretty-printing.
 * @param {r5js.IEnvironment} enclosingEnv The enclosing environment, if any.
 * @constructor
 * @implements {r5js.IEnvironment}
 */
r5js.Environment = function(name, enclosingEnv) {
    /**
     * @type {string}
     */
    this.name = name;

    if (enclosingEnv) {
        this.enclosingEnv = enclosingEnv;
        if (enclosingEnv instanceof RootEnvironment) {
            (/** @type {!RootEnvironment} */ (
                enclosingEnv)).setLookaside(this);
        }
        // useful for debugging console.log('created env ' + this + ' referencing ' + enclosingEnv);
    }
    this.bindings = {}; // hey, never use this; use this.get() instead
    this.closures = {};  // See Environment.prototype.addClosure
};

// See comments in Environment.prototype.addBinding.
r5js.Environment.prototype.unspecifiedSentinel = (function() {
    var ans = new Datum();
    ans.type = ans.payload = null;
    return ans;
}());

/** @override */
r5js.Environment.prototype.seal = function() {
    this.sealed = true;
};

r5js.Environment.prototype.allowRedefs = function() {
    this.redefsOk = true;
    return this;
};

r5js.Environment.prototype.clone = function(name) {

      if (this.enclosingEnv)
        throw new r5js.InternalInterpreterError('clone should only be used during '
        + 'interpreter bootstrapping');

    var cloned = new r5js.Environment(name, null);

    for (var name_ in this.bindings) {
        var val = this.bindings[name_];
        cloned.bindings[name_] = val instanceof SchemeMacro
            ? val.clone(cloned)
            : val;
    }

    return cloned;
};

/** @override */
r5js.Environment.prototype.hasBinding = function(name) {
    // We must never store null or undefined as a value.
    return this.bindings[name] != null;
};

/** @override */
r5js.Environment.prototype.hasBindingRecursive = function(
    name, searchClosures) {
    return this.hasBinding(name)
        || (searchClosures && this.closures[name])
        || (this.enclosingEnv && this.enclosingEnv.hasBindingRecursive(name, searchClosures));
};

/** @override */
r5js.Environment.prototype.get = function(name) {

    var maybe = this.bindings[name];

    if (maybe != null) {

        // Redirects for free ids in macro transcriptions
        if (maybe instanceof r5js.Environment)
            return maybe.get(name);
        /* We store primitive and non-primitive procedures unwrapped,
         but wrap them in a Datum if they are requested through get.
         (getProcedure, which is intended just for evaluating the operator
         on the trampoline, will return the unwrapped procedures.) */
        else if (typeof maybe === 'function'
            || maybe instanceof SchemeProcedure)
            return newProcedureDatum(name, maybe);
        else if (maybe === this.unspecifiedSentinel)
            return maybe;
        else if (maybe instanceof Datum)
            return newDatumRef(maybe);
        // Everything else
        else return maybe;
    } else if (maybe = this.closures[name]) {
        /* I think this is only used for ProcCall.prototype.cpsify, where
         identifiers are used to keep track of things while the structure
         is changed. Semantic use of procedures should be gated by
         Environment.prototype.getProcedure, and since that doesn't check
         the closures map, there should be no danger of accidentally
         returning a closure. */
        return maybe;
    }
    // If the current environment has no binding for the name, look one level up
    else if (this.enclosingEnv)
        return this.enclosingEnv.get(name);
    else
        throw new r5js.UnboundVariable(name + ' in env ' + this.name);
};

/** @override */
r5js.Environment.prototype.getProcedure = function(name) {
    var maybe = this.bindings[name];

    if (maybe != null) {
        if (maybe instanceof r5js.Environment) {
            return maybe.getProcedure(name);
        } else if (typeof maybe === 'function'
            || maybe instanceof SchemeProcedure
            || maybe instanceof SchemeMacro
            || maybe instanceof Continuation
            || maybe instanceof JsObjOrMethod) {
            return maybe;
        } else throw new r5js.EvalError('expected procedure, given ' + name);
    } else if (this.enclosingEnv)
        return this.enclosingEnv.getProcedure(name);
    else
        return null;
};

/**
 * @param {!r5js.Environment} other Environment to add closures from.
 * See comment to {@link r5js.IEnvironment.addClosure}.
 */
r5js.Environment.prototype.addClosuresFrom = function(other) {
    /* todo bl: we have to clone the SchemeProcedures to prevent
     some kind of infinite loop. I'm not entirely clear about what loop, though,
     since SchemeProcedure.prototype.cloneWithEnv itself does not do a lot
     of copying. */
    for (var name in other.closures)
        this.addBinding(name, other.closures[name].cloneWithEnv(this));
    return this;
};



/** @override */
r5js.Environment.prototype.addClosure = function(name, proc) {
    if (this.sealed) {
        throw new r5js.InternalInterpreterError('tried to bind '
            + name
            + ' in sealed environment '
            + this.name);
    } else if (!(proc instanceof SchemeProcedure)) {
        throw new r5js.InternalInterpreterError('invariant incorrect');
    } else if (this.closures[name]) {
        throw new r5js.InternalInterpreterError('invariant incorrect');
    } else {
        this.closures[name] = proc;
    }
};

/** @override */
r5js.Environment.prototype.addBinding = function(name, val) {

    if (this.sealed) {
        throw new r5js.InternalInterpreterError('tried to bind ' + name + ' in sealed environment ' + this);
    }

    else if (this.bindings[name] == null || this.redefsOk || name.charAt(0) === '@') {

        // useful for debugging if (val instanceof Datum)
        //    console.log(this + ' addBinding ' + name + ' = ' + val);

        /* Macros require a backlink to the environment they were defined in to resolve
         literal identifiers. todo bl: is there a better place to put this? */
        if (val instanceof SchemeMacro)
            val.definitionEnv = this;

        // Store primitive values directly.
        if (typeof val === 'number'
             || typeof val === 'string'
             || val === true
             || val === false) {
             this.bindings[name] = val;
         } else if (val === null) {
            /* A value of null on the trampoline means an unspecified value.
             For example, the JavaScript implementation of display returns null.
             In order to distinguish between an unbound variable (error) and
             a variable bound to an unspecified value (not an error), we use
             r5js.Environment.prototype.unspecifiedSentinel. */
            this.bindings[name] = this.unspecifiedSentinel;
        } else if (typeof val === 'function' /* primitive procedure */
            || val instanceof SchemeProcedure /* non-primitive procedure */
            || val instanceof Continuation /* call-with-current-continuation etc. */
            || val instanceof Array /* values and call-with-values */
            || val instanceof SchemeMacro /* macros */
            || val instanceof r5js.Environment /* Redirects for free ids in macro transcriptions */
            || val instanceof JsObjOrMethod /* JavaScript interop (experimental!) */) {
            this.bindings[name] = val;
        } else if (val instanceof Datum) {
        // lots of stuff, including wrapped procedures
            if (val.isVector() && !val.isArrayBacked())
                this.bindings[name] = val.convertVectorToArrayBacked();
            /* r5js.Environment.prototype.get should honor requests to store
             both unwrapped procedures (= JavaScript functions and
             SchemeProcedure objects) and those things wrapped in a Datum.
             In the latter case, we should store the unwrapped thing.

             We can see the first pathway in the desugar functions
             for procedures: they explicitly call addBinding with
             a SchemeProcedure as the second argument. The second pathway
             happens on the trampoline, for example:

             (define foo +)

             r5js.Environment.protoype.get will return + wrapped in a Datum,
             then the trampoline will call
             r5js.Environment.prototype.addBinding with this wrapped procedure
             as the second argument. */
            else
                this.bindings[name] = val.unwrap();
        } else {
            throw new r5js.InternalInterpreterError('tried to store '
                + name
                + ' = '
                + val
                + ', which is not an acceptable value');
        }
    } else {
        throw new r5js.InternalInterpreterError('redefining '
            + name
            + ' in same env, not allowed');
    }
};

/** @override */
r5js.Environment.prototype.toString = function() {
    return this.name;
};



/** @override */
r5js.Environment.prototype.mutate = function(name, newVal, isTopLevel) {
    var maybeBinding = this.bindings[name];
    if (maybeBinding != null || isTopLevel) {
        if (maybeBinding instanceof r5js.Environment) {
            maybeBinding.mutate(name, newVal, isTopLevel);
        } else {
            this.bindings[name] = null;
            this.addBinding(name, newVal);
        }
    } else if (this.enclosingEnv) {
        this.enclosingEnv.mutate(name, newVal, isTopLevel);
    } else throw new r5js.UnboundVariable(name);
};
