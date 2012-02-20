/* An Environment stores three common kinds of objects:
    - Datums (most Scheme values: numbers, identifiers, etc.)
    - SchemeProcedures (native Scheme procedures)
    - JavaScript functions ('primitive' Scheme procedures)

    There is a fourth kind of object, a Continuation, which can get stored
    when calling "magical" procedures like call/cc, where the current
    continuation is bound to a formal parameter.

    Environment.prototype.get will only ever return Datums and Continuations;
    it will wrap SchemeProcedures and JavaScript functions in Datum
    wrappers before returning, to allow for things like

    (cons + (lambda () "hi!"))

    A drawback is that comparisons on stuff retrieved from an Environment
    may need to be unwrapped:

    var x = env.get('foo');
    var y = env.get('foo');
    x == y // false if foo is a SchemeProcedure or JavaScript function!

    If you know your key should retrieve a SchemeProcedure or JavaScript
    function, you can use Environment.prototype.getProcedure to avoid the
    wrapping and unwrapping.
 */
function Environment(name, enclosingEnv) {
    this.name = name; // just for use in pretty-printing
    if (enclosingEnv) {
        this.enclosingEnv = enclosingEnv;
        // useful for debugging console.log('created env ' + this + ' referencing ' + enclosingEnv);
    }
    this.bindings = {}; // hey, never use this; use this.get() instead
}

// See comments in Environment.prototype.addBinding.
Environment.prototype.unspecifiedSentinel = new Object();

/* Just for environments defined in the standard; users shouldn't be able to
    add to them. */
Environment.prototype.seal = function() {
    this.sealed = true;
};

Environment.prototype.allowRedefs = function() {
    this.redefsOk = true;
    return this;
};

Environment.prototype.clone = function(name) {

      if (this.enclosingEnv)
        throw new InternalInterpreterError('clone should only be used during '
        + 'interpreter bootstrapping');

    var cloned = new Environment(name);

    for (var name in this.bindings) {
        var val = this.bindings[name];
        cloned.bindings[name] = val instanceof SchemeMacro
            ? val.clone(cloned)
            : val;
    }

    return cloned;
};

/* Intended just to be used as a sanity check during startup,
 to make sure we don't multiply define builtin procedures. */
Environment.prototype.hasBinding = function(name) {
    /* This is not a bug, since we store values that could confuse JavaScript,
        like #f and 0, inside datum objects. */
    return this.bindings[name];
};

Environment.prototype.hasBindingRecursive = function(name) {
    /* This won't foul up because bindings can't have primitives like false
        that could confuse the comparison (they're always wrapped in datums) */
    return this.bindings[name]
        || (this.enclosingEnv && this.enclosingEnv.hasBindingRecursive(name));
};

Environment.prototype.ancestors = function(array) {
    if (!array)
        array = [];
    array.unshift(this);
    return this.enclosingEnv
        ? this.enclosingEnv.ancestors(array)
        : array;
};

Environment.prototype.leastCommonAncestor = function(other) {
  var myAncestors = this.ancestors();
    var otherAncestors = other.ancestors();
    var stop = myAncestors.length < otherAncestors.length
        ? myAncestors.length
        : otherAncestors.length;

    var ans = null;

    for (var i=0; i < stop; ++i) {
        if (myAncestors[i] !== otherAncestors[i])
            break;
        else
            ans = myAncestors[i];
    }

    return ans;
};

Environment.prototype.addAllRecursive = function(other) {
    for (var name in this.bindings)
        throw new InternalInterpreterError('invariant incorrect');

    /* Clearly, we should return immediately if this === other.
     But we should also return immediately if other is the direct parent
     of this; since addAllRecursive is only used on brand-new Environments,
     it would be impossible for this to have any binding different from its
     direct parent. */
    if (other === this || other === this.enclosingEnv)
        return this;
    var stop = this.leastCommonAncestor(other);

    do {
        this.addAll(other);
    } while (other !== stop
        && (other = other.enclosingEnv));

    /* We return a new Environment pointing back to this because if any of
     the addAlls bound anything, a subsequent bind in this environment to the
     same identifier would raise an error. */
    return new Environment(this.name /* might be misleading... */, this);
};

Environment.prototype.addAll = function(otherEnv) {
    var otherBindings = otherEnv.bindings;
    for (var name in otherBindings) {
        /* Since addAllRecursive calls addAll from the leaves to the root,
         we don't want to accidentally overwrite a fresher (descendant)
         binding with an older (ancestor) binding. Also, we add an indirection
         to the environment, rather than copy the value itself, so that
         mutations work properly. */
        if (!this.hasBinding(name))
            this.bindings[name] = otherEnv;
    }

    return this;
};

Environment.prototype.get = function(name, disableDatumClone) {

    var maybe = this.bindings[name];

    if (maybe) {
        // Redirects for free ids in macro transcriptions
        if (maybe instanceof Environment)
            return maybe.get(name);
        /* Wrap primitive procedures in a Datum. We could store primitive
         procedures already wrapped. But if getProcedure() is expected
         to be more common for primitive procedures than get(), I think it
         is better to only wrap them for get(). Only profiling can say. */
        else if (typeof maybe === 'function')
            return newProcedureDatum(name, maybe);
        else if (maybe === this.unspecifiedSentinel)
            return null;
        else if (maybe instanceof Datum && !disableDatumClone) {
            /* Any Datum that is going to make it back out to the trampoline
             must be defensively cloned to prevent Datum cycles. For example,
             if the Environment gave back the exact same Datum object X for
             both of the following x's:

             (define x 1)
             (list x x)

             then the trampoline would set X.nextSibling = X.

             However, Scheme has a handful of primitive mutation procedures:
             set-car!, set-cdr!, string-set!, and vector-set!. For the first
             three procedures, we remember where the defensive clone
             came from, so if the procedures are called on a defensive clone,
             we can also call them on the original. (For vector-set!, we don't
             need to do anything. Datums representing vectors hold pointers
             to JavaScript arrays, and these pointers accomplish what we're
             doing here for the other data types.)

             An alternative and attractive idea is to unwrap everything
             before storing, and adapt the trampoline and primitive
             procedures to operate on unwrapped values as far as possible.
             I toyed around with this for a bit, but the wrapping/unwrapping
             code seemed to proliferate and make APIs less uniform. It may
             be possible to do correctly, however. */

            return maybe.couldBeMutated()
                ? maybe.clone(true).setCloneSource(maybe)
                : maybe.clone(true);
        }
        // Everything else
        else return maybe;
    }
    // If the current environment has no binding for the name, look one level up
    else if (this.enclosingEnv)
        return this.enclosingEnv.get(name, disableDatumClone);
    else
        throw new UnboundVariable(name + ' in env ' + this.name);
};

Environment.prototype.getProcedure = function(name) {
    var maybe = this.bindings[name];

    if (maybe) {
        if (maybe instanceof Environment)
            return maybe.getProcedure(name);
        else if (maybe instanceof Datum && maybe.isProcedure()) {
            if (maybe.hasClosure())
                maybe.payload.env = maybe.closure;
            return maybe.payload;
        } else if (typeof maybe === 'function'
            || maybe instanceof SchemeMacro
            || maybe instanceof Continuation) {
            return maybe;
        } else throw new EvalError('expected procedure, given ' + name);
    } else if (this.enclosingEnv)
        return this.enclosingEnv.getProcedure(name);
    else
        return null;
};

Environment.prototype.addBinding = function(name, val) {

    if (this.sealed) {
        throw new InternalInterpreterError('tried to bind ' + name + ' in sealed environment ' + this);
    }

    else if (!this.bindings[name] || this.redefsOk || name.charAt(0) === '@') {

        // useful for debugging if (val instanceof Datum)
        //    console.log(this + ' addBinding ' + name + ' = ' + val);

        /* Macros require a backlink to the environment they were defined in to resolve
         literal identifiers. todo bl: is there a better place to put this? */
        if (val instanceof SchemeMacro)
            val.definitionEnv = this;

        if (val === null) {
            /* A value of null on the trampoline means an unspecified value.
             For example, the JavaScript implementation of display returns null.
             In order to distinguish between an unbound variable (error) and
             a variable bound to an unspecified value (not an error), we use
             Environment.prototype.unspecifiedSentinel. I suppose we could
             bind null or undefined, but this would probably lead to bugs in
             conditionals (if (this.bindings[env]) ...) */
            this.bindings[name] = this.unspecifiedSentinel;
        } else if (val instanceof SchemeProcedure) { /* non-primitive procedure */
            this.bindings[name] = newProcedureDatum(name, val);
        } else if (typeof val === 'function' /* primitive procedure */
            || val instanceof Continuation /* call-with-current-continuation etc. */
            || val instanceof Array /* values and call-with-values */
            || val instanceof SchemeMacro /* macros */
            || val instanceof Environment /* Redirects for free ids in macro transcriptions */) {
            this.bindings[name] = val;
        } else if (val instanceof Datum) {
            // Make sure we are not dealing with a clone.
            val = val.getCloneSource();
        /* lots of stuff, including wrapped procedures
             (We need wrapped procedures to support returning
             SchemeProcedure objects into different environments. The Datum
             wrapper has a backlink to the closure in which it was created.) */
            if (val.isVector() && !val.isArrayBacked())
                this.bindings[name] = val.convertVectorToArrayBacked();
            else
                this.bindings[name] = val;
        } else {
            throw new InternalInterpreterError('tried to store '
                + name
                + ' = '
                + val
                + ', which is not an acceptable value');
        }
    } else {
        throw new InternalInterpreterError('redefining '
            + name
            + ' in same env, not allowed');
    }
};

Environment.prototype.rootEnv = function() {
    return this.enclosingEnv && !this.enclosingEnv.sealed
        ? this.enclosingEnv.rootEnv()
        : this;
};

Environment.prototype.toString = function() {
    return this.name;
};

/* R5RS 5.2.1: "At the top level of a program, a definition

 (define <variable> <expression>)

 has essentially the same effect as the assignment expression

 (set! <variable> <expression>)

 if <variable> is bound. If <variable> is not bound, however, then
 the definition will bind <variable> to a new location before performing
 the assignment, whereas it would be an error to perform a set! on
 an unbound variable."

 We use the isTopLevel parameter to perform the override mentioned. */
Environment.prototype.mutate = function(name, newVal, isTopLevel) {
    var maybeBinding = this.bindings[name];
    if (maybeBinding || isTopLevel) {
        if (maybeBinding instanceof Environment) {
            maybeBinding.mutate(name, newVal, isTopLevel);
        } else {
            this.bindings[name] = null;
            this.addBinding(name, newVal);
        }
    } else if (this.enclosingEnv) {
        this.enclosingEnv.mutate(name, newVal, isTopLevel);
    } else throw new UnboundVariable(name);
};